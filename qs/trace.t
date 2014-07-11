local S = terralib.require("lib.std")
local globals = terralib.require("globals")
local util = terralib.require("lib.util")



------------------------------------
---  Trace compilation machinery ---
------------------------------------

-- The program currently being compiled
local currentProgram = nil

-- The value of globals.real before compilation was invoked.
-- We restore this when compilation is finished.
local prevReal = nil

-- Compiling probabilistic programs proceeds in two passes.
-- The first pass detects the types of all random choices used in the program.
-- This flag indicates whether we are in the first pass.
local rcTypeDetectionPass = false

-- This is where we record the types of all random choices used.
local rcTypesUsed = {}

-- Signalling functions exposed to code in other files
local compilation = {}
function compilation.isCompiling()
	return currentProgram ~= nil
end
function compilation.currentlyCompilingProgram()
	return currentProgram
end
function compilation.beginCompilation(program, real)
	currentProgram = program
	prevReal = globals.real
	globals.real = real
end
function compilation.beginRCTypeDetectionPass()
	rcTypesUsed = {}
	rcTypeDetectionPass = true
end
function compilation.endRCTypeDetectionPass()
	rcTypeDetectionPass = false
end
function compilation.endCompilation()
	currentProgram = nil
	globals.real = prevReal
end



------------------------------------
---  Address tracking machinery  ---
------------------------------------


-- Set to true during a trace update
local isRecordingTrace = global(bool, false)

-- Set to false during a trace update where control flow structure
--    is known to be fixed. 
local canStructureChange = global(bool, true)

-- Stack of probabilistic function callsite IDs that provide
--    structural information to the execution trace.
local addressStack = global(S.Vector(int))
addressStack:__init()

-- Pushing/popping/incrementing the address stack
local nextAddress = 0
local function pushAddressStack()
	local address = nextAddress
	nextAddress = nextAddress + 1
	return quote
		addressStack:insert(address)
	end
end
local function popAddressStack()
	return quote
		addressStack:remove()
	end
end

-- A probabilistic function is a Terra function that makes random choices
--    (or whose callees make random choices)
-- They can be defined as:
--    local f = qs.func(terrafn)
-- Recursive functions can be defined as:
--    local f = qs.func()
--    f:define(terrafn)
local function getFuncReturnType(terrafn)
	local RetType = nil
	for _,d in ipairs(terrafn:getdefinitions()) do
		-- Check if the definition has an explicit return type.
		-- If it does not, attempt to asynchronously compile it.
		-- If this does not return the type, then we know the definition is recursive.
		local success, ftype = d:peektype()
		if not success then ftype = d:gettype(true) end
		if not ftype then
			error("Recursive definition of a qs.func must have explicit return type")
		end
		-- All overloads of the function must have the same return type
		--    (subsequent code generation assumes this)
		if RetType == nil then
			RetType = ftype.returntype
		else
			assert(ftype.returntype == RetType,
				"All overloads of qs.func must have the same return type")
		end
	end
	return RetType
end
local function _func(terrafn, ismethod)
	local data = { def = nil }

	local qsfunc = macro(function(...)
		local args = terralib.newlist({...})
		local argtypes = args:map(function(x) return x:gettype() end)
		-- If we're wrapping a method, ensure that the first argument (i.e. self)
		--    is pointer-to-struct
		if ismethod and (not argtypes[1]:ispointertostruct()) then
			args[1] = `&args[1]
			argtypes[1] = `&argtypes[1]
		end
		local argstmp = argtypes:map(function(t) return symbol(t) end)
		local RetType = getFuncReturnType(terrafn)
		return quote
			var result : RetType
			-- If we're not recording a trace, or if we're guaranteed no
			--    structure change, then skip address tracking
			if not isRecordingTrace or not canStructureChange then
				result = [data.def]([args])
			else
				var [argstmp] = [args]
				[pushAddressStack()]
				result = [data.def]([argstmp])
				[popAddressStack()]
			end
		in
			result
		end
	end)

	qsfunc.data = data

	qsfunc.define = function(self, terrafn)
		if terrafn ~= nil then
			assert(terralib.isfunction(terrafn),
				"Definition of a qs.func must be a Terra function")
			self.data.def = terrafn
		end
	end

	qsfunc:define(terrafn)

	return qsfunc
end
local function func(terrafn)
	return _func(terrafn, false)
end
local function method(terramethod)
	return _func(terramethod, true)
end


-------------------------------------
---  Random execution trace type  ---
-------------------------------------




-------------------------------------
---       Public interface        ---
-------------------------------------


-- Look up random choice value in the currently-executing trace
-- (Does address stack management...)
-- IMPORTANT: This should record that this program is using this RandomChoice type.
--            Non-POD values should be returned by pointer
local function lookupRandomChoice(RandomChoiceT, args, ctoropts, updateopts)
	-- TODO: Actually implement this.
	RandomChoiceT.methods.__init:compile()
	RandomChoiceT.methods.update:compile()
	RandomChoiceT.methods.rescore:compile()
	RandomChoiceT.methods.proposal:compile()
	return quote
		var rc : RandomChoiceT
		rc:init([args], [ctoropts])
		-- rc:update([args], [updateopts])
		var val = rc:getValue()
	in
		[util.isPOD(RandomChoiceT.ValueType) and (`val) or (`&val)]
	end
end

local factor = macro(function(num)
	-- TODO: Actually implement this.
	return quote
		var x = num
	end
end)


-- TODO: condition, factorfunc, range


return 
{
	compilation = compilation,
	isRecordingTrace = isRecordingTrace,
	canStructureChange = canStructureChange,
	lookupRandomChoice = lookupRandomChoice,
	factor = factor,
	exports = 
	{
		factor = factor,
		func = func,
		method = method
	}
}




