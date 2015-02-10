local S = require("qs.lib.std")
local trace = require("qs.trace")
local qs = require("qs.globals")


-- A program is the entity that quicksand performs inference on.
-- It wraps up a Lua function that encapsulates and returns a Terra function.
-- Its one function, 'compile', compiles and returns the program for the given
--    choice of real (optional) as well as the return type of the program

local typeDetectionPass = S.memoize(function(self, real)
	trace.compilation.beginRCTypeDetectionPass()
	local tfn = self.specializationFn(self, real, true)
	if not terralib.isfunction(tfn) then
		error("Lua function passed to a qs.program must return a Terra function")
	end
	tfn:compile()
	local T = tfn:gettype()
	if #T.parameters ~= 0 then
		error("Terra function returned by a qs.program must take no arguments")
	end
	local RetType = T.returntype
	trace.compilation.endRCTypeDetectionPass()
	return RetType
end)

local progcompile = S.memoize(function(self, real)

	trace.compilation.beginCompilation(self, real)

	-- Do a first compilation pass that detects
	--    the types of all random choices used in the program.
	local RetType = typeDetectionPass(self, real)

	-- Now, compile the actual program.
	-- ABSOLUTELY MUST be done asynchronously, since compiling
	--    the program requires us to compile a trace type, but
	--    compiling the trace type requires us to compile the program...
	local tfn = self.specializationFn(self, real, false)
	tfn:compile(function() trace.compilation.endCompilation() end)

	return { prog=tfn, RetType=RetType }
end)

local programmt =
{
	compile = function(self, optreal)
		local real = optreal or qs.real
		local progAndRetType = progcompile(self, real)
		return progAndRetType.prog, progAndRetType.RetType
	end
}
programmt.__index = programmt

local function program(luafn)
	if type(luafn) ~= "function" then
		error("Argument to a qs.program must be a Lua function")
	end
	local progobj = 
	{
		specializationFn = S.memoize(function(prog, real, isTypeDetectPass)
			return luafn()
		end)
	}
	setmetatable(progobj, programmt)
	return progobj
end

local function isprogram(x)
	return getmetatable(x) == programmt
end

local function assertIsProgram(program, caller)
	assert(isprogram(program),
		string.format("'program' argument to '%s' must be a qs.program", caller))
end



-- A module encapsulates (potentially probabilistic) code
--    used by a probabilistic program.
-- Like a program, it wraps up a Lua function, but this time the Lua function
--    can return anything (usually a table of Terra types and functions)
-- The 'open' method can be called from within a program (or another module)
--    to get the module's returned contents.
-- Outside of a program (or other module), the 'openAs(program)' method must
--    be used instead. This is because the compilation process for probabilistic
--    code is specialized to the program using that code.

local function modCacheLookup(mod, prog, real, typeDetectPass)
	local p = mod.cache[prog]
	if not p then return false end
	local r = p[real]
	if not r then return false end
	local t = r[typeDetectPass]
	if not t then return false end
	return t
end

local function modCachePut(mod, prog, real, typeDetectPass, value)
	local p = mod.cache[prog]
	if not p then
		mod.cache[prog] = {}
		p = mod.cache[prog]
	end
	local r = p[real]
	if not r then
		p[real] = {}
		r = p[real]
	end
	r[typeDetectPass] = value
end

local function modCacheLookupOrCreate(mod, prog, real, typeDetectPass)
	local res = modCacheLookup(mod, prog, real, typeDetectPass)
	if not res then
		res = mod.specializationFn()
		modCachePut(mod, prog, real, typeDetectPass, res)
	end
	return res
end

local modulemt = 
{
	-- open():
	-- Checks if a program is currently compiling, throws error if not.
	-- Invokes the specialization function, memoizes them under currProgram and qs.real
	open = function(self)
		if not trace.compilation.isCompiling() then
			error("Cannot call module.open() outside of a probabilistic program")
		end
		local currProg = trace.compilation.currentlyCompilingProgram()
		local typeDetectPass = trace.compilation.isDoingTypeDetectionPass()
		return modCacheLookupOrCreate(self, currProg, qs.real, typeDetectPass)
	end,

	-- openAs(program, [real])
	-- Checks memo cache
	-- If cache miss, invokes program:compile(real)
	-- If still cache miss, then throw an error
	openAs = function(self, prog)
		assertIsProgram(prog, "module.openAs")
		local val = modCacheLookup(self, prog, qs.real, false)
		if not val then
			prog:compile(qs.real)
			val = modCacheLookup(self, prog, qs.real, false)
			if not val then
				error("module:openAs(program) -- module must be used in program.")
			end
		end
		return val
	end
}
modulemt.__index = modulemt

local function module(luafn)
	if type(luafn) ~= "function" then
		error("Argument to a qs.module must be a Lua function")
	end
	local modobj = 
	{
		specializationFn = luafn,
		cache = {}
	}
	setmetatable(modobj, modulemt)
	return modobj
end

local function ismodule(x)
	return getmetatable(x) == modulemt
end




return
{
	program = program,
	isprogram = isprogram,
	assertIsProgram = assertIsProgram,
	module = module,
	ismodule = ismodule,
	exports = 
	{
		program = program,
		module = module
	}
}



