local util = terralib.require("qs.lib.util")

local S = util.require("lib.std")
local globals = util.require("globals")
local Hash = util.require("lib.hash")
local HashMap = util.require("lib.hashmap")



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
local rcTypesUsed = terralib.newlist()

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
	rcTypesUsed = terralib.newlist()
	rcTypeDetectionPass = true
end
function compilation.endRCTypeDetectionPass()
	rcTypeDetectionPass = false
end
function compilation.endCompilation()
	currentProgram = nil
	globals.real = prevReal
end



-------------------------------------
---  Random execution trace type  ---
-------------------------------------

local Address = S.Vector(int)
local terra hashAddress(addr: Address)
	return Hash.rawhash([&int8](addr:get(0)), sizeof(int)*addr:size())
end
hashAddress:setinlined(true)

-- Database of values for a particular type of random choice
local RandomDB = S.memoize(function(RandomChoiceT)

	local struct ChoicesWithCounter(S.Object)
	{
		choices: S.Vector(RandomChoiceT),
		counter: uint64
	}
	terra ChoicesWithCounter:__init()
		self:initmembers()
		self.counter = 0
	end

	local struct ChoicePointersWithCounter(S.Object)
	{
		pointers: S.Vector(&RandomChoiceT),
		counter: uint64
	}
	terra ChoicePointersWithCounter:__init()
		self:initmembers()
		self.counter = 0
	end

	-- Stores both a structured map from addresses to choices as well
	--    as a flat list of choices.
	-- Can use the second when we know the control flow structure of the
	--    program is not changing.
	local struct RandomDBT(S.Object)
	{
		addressStack: &Address,
		-- IMPORTANT: It is ok to take pointers directly into choicemap because HashMap
		--    is implemented using a table of linked lists, and mapped values live at the
		--    same memory location (i.e. list cell) forever after they're inserted.
		-- If and when we change the implementation of HashMap, we'll need to ensure that
		--    this is still OK (or make appropriate modifications).
		-- For example: If we use a more efficient, non-link-list-based table, we'll need
		--    to store pointers to RandomChoiceT that are allocated elsewhere.
		-- An idea: Allocate the RandomChoiceT's from a custom memory pool which we can
		--    optionally re-order. If structural changes are uncommmon, then
		--    after a structure change, we can re-order the memory pool by linear program
		--    occurrence order so that we get greater cache coherence when accessing random
		--    choices via the flat list.
		choicemap: HashMap(Address, ChoicesWithCounter, hashAddress),
		choicelist: ChoicePointersWithCounter
	}

	terra RandomDBT:__init(addressStack: &Address)
		self:initmembers()
		self.addressStack = addressStack
	end

	terra RandomDBT:lookupNonStructural()
		-- Just retrieve the next choice in the flat list, and then
		--   increment the index.
		var x = self.choicelist.pointers(self.choicelist.counter)
		self.choicelist.counter = self.choicelist.counter + 1
		return x
	end

	-- Returns the random choice, plus a boolean indicating whether the choice
	--    was retrieved (true) or had to be created (false).
	local function lookupStructural(initArgs)
		return quote
			-- Retrieves the list of choices made at this lexical address (or creates an
			--    empty list if no choices have yet been made here)
			var clist, foundit = self.choicemap:getOrCreatePointer(@self.addressStack)
			-- If we've hit this lexical address more times than we have stored choices, then
			--    we need to create a new database entry for the new choice.
			if clist.choices:size() <= clist.counter then
				foundit = false
				-- Add a new DB entry
				clist.choices:insert()
				clist.choices(clist.choices:size()):init([initArgs])
				-- Increment self.newlogprob, since we just created a new random choice
				self.newlogprob = self.newlogprob + c.choices(clist.choices:size()).logprob
			end
			var x = self.clist.choices:get(clist.counter)
			-- Record the fact that we've hit this lexical position one more time
			clist.counter = clist.counter + 1
			-- Add a pointer to this choice to the flat list that we're building up.
			self.choicelist.pointers:insert(x)
		in
			x, foundit
		end
	end

	-- Make a method for each overload of RandomChoiceT:init
	for _,def in ipairs(RandomChoiceT.methods.__init:getdefinitions()) do
		local argsyms = def:gettype().parameters:map(function(t) return symbol(t) end)
		terra RandomDBT:lookupStructural([argsyms])
			return [lookupStructural(argsyms)]
		end
	end

	-- Clear out everything
	terra RandomDBT:clear()
		self.choicemap:clear()
		self.choicelist:clear()
	end

	-- Prepare for a trace update
	terra RandomDBT:prepareForTraceUpdate(canStructureChange: bool)
		-- If non-structural:
		--    reset the flat list index counter
		if not canStructureChange then
			self.choicelist.counter = 0
		-- If structural:
		--    mark all variables as inactive
		--    clear the flat choice list
		--    reset the per-address loop counters
		else
			for rcp in self.choicelist.pointers do rcp.active = false end
			self.choicelist.pointers:clear()
			for add,clist in self.choicemap do clist.counter = 0 end
		end
	end

	-- Clear out any choices that were not reachable on the last run of the program
	-- (These are the ones left marked 'inactive') 
	-- Returns the summed logprob of all removed choices
	terra RandomDBT:clearUnreachables()
		var oldlp : RandomChoiceT.RealType = 0.0
		for addr,clist in self.choicemap do
			for i=[int64](clist.size()-1),-1,-1 do
				if not clist(i).active then
					oldlp = oldlp + clist(i).logprob
					S.rundestructor(clist(i))
					clist:remove(i)	-- Does not destruct; instead returns the removed value
					i = i + 1
				end
			end
		end
		return oldlp
	end

	return RandomDBT

end)

-- The trace type itself
local RandExecTrace = S.memoize(function(program, real)

	-- Compile the program
	-- (This is an asynchronous operation that will not finish until this type constructor
	--    returns. However, it will synchronously compute the 'rcTypesUsed' list and determine
	--    the return type of program)
	local tprog, RetType = program:compile(real)

	local struct RandExecTraceT(S.Object)
	{
		logprob: real,
		newlogprob: real,
		oldlogprob: real,
		conditionsSatisfied: bool,
		returnValue: RetType,
		-- This starts out false, but is forever after set to true as soon as :update() is run
		-- IMPORTANT: Everything works out fine, provided the destructor is never invoked before
		--    the first run of :update(). I don't think this should ever happen.
		hasReturnValue: bool,

		addressStack: Address,
		canStructureChange: bool
	}

	-- Add a RandomDB member for every type of random choice used
	--   by the program.
	local rcTypeIndices = terralib.newlist()
	for i,rct in ipairs(rcTypesUsed) do
		rcTypeIndices[rct] = i
		RandExecTraceT.entries:insert({ field=string.format("rdb%d", i), type=RandomDB(rct) })
	end
	local function rdbForType(self, RCType)
		return `self.[string.format("rdb%d", rcTypeIndices[RCType])]
	end

	terra RandExecTraceT:__init()
		self.logprob = 0.0
		self.newlogprob = 0.0
		self.oldlogprob = 0.0
		self.conditionsSatisfied = false
		self.hasReturnValue = false

		self.addressStack:init()
		self.canStructureChange = true

		escape
			for _,rct in ipairs(rcTypesUsed) do
				emit quote
					[rdbForType(self, rct)]:init(&self.addressStack)
				end
			end
		end

		while not self.conditionsSatisfied do
			-- Clear out all random dbs
			escape
				for _,rct in ipairs(rcTypesUsed) do
					emit quote
						[rdbForType(self, rct)]:clear()
					end
				end
			end
			-- Try running from scratch
			self:update(true)
		end
	end

	local nextAddress = 0
	RandExecTraceT.methods.pushAddressStack = macro(function()
		local address = nextAddress
		nextAddress = nextAddress + 1
		return quote
			if self.canStructureChange then
				self.addressStack:insert(address)
			end
		end
	end)

	RandExecTraceT.methods.popAddressStack = macro(function()
		return quote
			if self.canStructureChange then
				self.addressStack:remove()
			end
		end
	end)

	-- Also returns a boolean indicating whether the random choice was
	--    retrieved or created.
	RandExecTraceT.methods.lookupRandomChoice = macro(function(RCType, ...)
		local initArgs = terralib.newlist({...})
		local initArgsTmp = initArgs:map(function(a) return symbol(a:gettype()) end)
		return quote
			var x : &RCType
			var foundit = true
			if not self.canStructureChange then
				x = [rdbForType(self, RCType)]:lookupNonStructural()
			else
				var [initArgsTmp] = [initArgs]
				self:pushAddressStack()
				x, foundit = [rdbForType(self, RCType)]:lookupStructural([initArgsTmp])
				self:popAddressStack()
			end
		in
			x, foundit
		end
	end)

	terra RandExecTraceT:update(canStructureChange: bool)
		self.canStructureChange = canStructureChange

		-- Assume ownership of the global trace
		var prevTrace = [globalTrace()]
		[globalTrace()] = self

		self.logprob = 0.0
		self.newlogprob = 0.0
		self.oldlogprob = 0.0
		self.conditionsSatisfied = true

		-- Prepare rdbs for trace update
		escape
			for _,rct in ipairs(rcTypesUsed) do
				emit quote
					[rdbForType(self, rct)]:prepareForTraceUpdate(canStructureChange)
				end
			end
		end

		-- Run computation
		if self.hasReturnValue then S.rundestructor(self.returnValue) end
		self.returnValue = tprog()
		self.hasReturnValue = true

		-- If structural, clear out unreachable variables from rdbs
		if canStructureChange then
			escape
				for _,rct in ipairs(rcTypesUsed) do
					emit quote
						self.oldlogprob = self.oldlogprob + [rdbForType(self, rct)]:clearUnreachables()
					end
				end
			end
		end

		-- Release ownership of global trace
		[globalTrace()] = prevTrace
	end

	return RandExecTraceT

end)

-- Create or retrieve a global variable pointing to the
--    currently executing trace for a particular program type.
local __globalTrace = S.memoize(function(program, real)
	return global(&RandomChoice(program, real), nil)
end)
local function globalTrace()
	assert(currentProgram,
		"Cannot call 'globalTrace' outside of trace compilation")
	return __globalTrace(currentProgram, globals.real)
end

-- Check whether the current execution of the program is a trace recording
--    execution
local function isRecordingTrace()
	return `[globalTrace()] ~= nil
end



-----------------------------------------------------
---  Address tracking for user-defined functions  ---
-----------------------------------------------------

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
				"All overloads of a qs.func must have the same return type")
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
			-- If we're not recording a trace then skip address tracking
			if not [isRecordingTrace()] then
				result = [data.def]([args])
			else
				var [argstmp] = [args]
				[globalTrace()]:pushAddressStack()
				result = [data.def]([argstmp])
				[globalTrace()]:popAddressStack()
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
---       Public interface        ---
-------------------------------------


-- Look up random choice value in the currently-executing trace
-- Non-POD values are returned by pointer
local function lookupRandomChoiceValue(RandomChoiceT, args, ctoropts, updateopts)
	-- If we're doing the random choice type detection compiler pass, then we
	--    record the use of this type
	if rcTypeDetectionPass then
		rcTypesUsed:insert(RandomChoiceT)
	end
	return quote
		var val : RandomChoiceT.ValueType
		-- Only proceed with trace lookup if we're past the type detection pass
		escape
			if not rcTypeDetectionPass then
				emit quote
					var rc, foundit = [globalTrace()]:lookupRandomChoice(RandomChoiceT, [args], [ctoropts])
					-- If this choice was retrieved, not created, then we should check if
					--    the prior probability etc. need to be updated
					if foundit then
						rc:update([args], [updateopts])
					end
					-- Regardless, we need to increment the trace's log probability
					[globalTrace()].logprob = [globalTrace()].logprob + rc.logprob

					val = rc:getValue()
				end
			end
		end
	in
		[util.isPOD(RandomChoiceT.ValueType) and (`val) or (`&val)]
	end
end

-- 'factor' and 'factorfunc' allow direct increment of the currently-executing trace's log probability
-- 'factor' takes the logprob increment amount directly
-- 'factorfunc' wraps a Terra function whose return value should be used as the increment amount
-- TODO: Provide 'isEvaluatingFactors' which can be toggled off to turn off evaluation of
--    (expensive) factors when we're only running the trace to e.g. reconstruct a return value.
local factor = macro(function(num)
	return quote
		if [isRecordingTrace()] then
			[globalTrace()].logprob = [globalTrace()].logprob + num
		end
	end
end)
local function factorfunc(fn)
	assert(terralib.isfunction(fn),
		"Argument to qs.factorfunc must be a Terra function")
	for _,def in fn:getdefinitions() do
		assert(def:gettype().returntype == globals.real,
			"Definition of qs.factorfunc must return type 'real'")
	end
	fn = func(fn)
	return macro(function(...)
		local args = terralib.newlist({...})
		return quote
			if [isRecordingTrace()] then
				[globalTrace()].logprob = [globalTrace()].logprob + fn([args])
			end
		end
	end)
end

-- 'condition' imposes a hard constraint on the program execution space.
-- 'conditionfunc' is like 'factorfunc', but for hard constraints.
--  TODO: Use 'isEvaluatingFactors' here, too?
local condition = macro(function(pred)
	return quote
		if [isRecordingTrace()] then
			[globalTrace()].conditionsSatisfied =
				[globalTrace()].conditionsSatisfied and pred
		end
	end
end)
local function conditionfunc(fn)
	assert(terralib.isfunction(fn),
		"Argument to qs.conditionfunc must be a Terra function")
	for _,def in fn:getdefinitions() do
		assert(def:gettype().returntype == bool,
			"Definition of qs.conditionfunc must return type 'bool'")
	end
	fn = func(fn)
	return macro(function(...)
		local args = terralib.newlist({...})
		return quote
			if [isRecordingTrace()] then
				[globalTrace()].conditionsSatisfied =
					[globalTrace()].conditionsSatisfied and fn([args])
			end
		end
	end)
end

-- 'range' takes a min and max value (integers) and constructs a range
--     object that can be iterated with a standard for loop.
--  It also pushes and pops the address stack, so using range-based loops
--     may provide better statistical efficiency when querying programs
--     with nested loops and structure change.
local struct __Range
{
	min: int64,
	max: int64
}
__Range.metamethods.__for = function(syms,iter,body)
	return syms, quote
		var it = iter
		if [isRecordingTrace()] then [globalTrace()]:pushAddressStack() end
		for [syms[1]] = it.min,it.max do
			body
		end
		if [isRecordingTrace()] then [globalTrace()]:popAddressStack() end
	end
end
local terra range(min: int64, max: int64)
	return __Range { min, max }
end
range:setinlined(true)


-- TODO: condition, factorfunc, range


return 
{
	compilation = compilation,
	isRecordingTrace = isRecordingTrace,
	lookupRandomChoiceValue = lookupRandomChoiceValue,
	factor = factor,
	exports = 
	{
		func = func,
		method = method,
		factor = factor,
		factorfunc = factorfunc,
		condition = condition,
		conditionfunc = conditionfunc,
		range = range
	}
}





