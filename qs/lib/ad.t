local S = require("qs.lib.std")
local MemoryPool = require("qs.lib.memoryPool")
local cmath = terralib.includec("math.h")


-- =============== GLOBALS ===============

-- Global memory pool
local memPool = global(MemoryPool)

-- Global stack of variables active for the current computation
local VoidPtr = &opaque
local AdjointFn = {VoidPtr} -> {}
local struct TapeEntry
{
	datum: VoidPtr,
	fn: AdjointFn
}
local tape = global(S.Vector(TapeEntry))

local terra initGlobals()
	memPool:init()
	tape:init()
end
initGlobals()


-- =============== DUAL NUMBER TYPE GENERATION ===============

-- Generate a list of expression that refer to sequential `anonymous'
-- fields in a struct instance
local function makeFieldExpList(obj, numFields)
	local fields = {}
	for i=1,numFields do
		table.insert(fields, `obj.[string.format("_%d", i-1)])
	end
	return fields
end

-- The inner dual number type
local DualNum = S.memoize(function(...)

	local struct DualNumT
	{
		val: double,	-- Value
		adj: double,	-- Adjoint
	}

	-- Add extra entries from the argument type list
	local numExtraFields = select("#",...)
	for i=1,numExtraFields do
		table.insert(DualNumT.entries, {field = string.format("_%d", i-1), type = (select(i,...))})
	end

	-- All dual nums are allocated from the memory pool
	DualNumT.methods.new = macro(function(val, adjFn, ...)
		local args = {}
		for i=1,numExtraFields do
			table.insert(args, (select(i, ...)))
		end
		return quote
			var dnptr = [&DualNumT](memPool:alloc(sizeof(DualNumT)))
			dnptr.val = val
			dnptr.adj = 0.0
			[makeFieldExpList(dnptr, numExtraFields)] = [args]
			if adjFn ~= nil then
				var tapeEntry : TapeEntry
				tapeEntry.datum = dnptr
				tapeEntry.fn = adjFn
				tape:insert(tapeEntry)
			end
		in
			dnptr
		end
	end)

	return DualNumT
end)
local DualNumBase = DualNum()


-- The externally-visible dual number type
local struct num
{
	impl: &DualNumBase
}
function num.metamethods.__typename() return "dualnum" end

terra num:val()
	return self.impl.val
end
num.methods.val:setinlined(true)

terra num:adj()
	return self.impl.adj
end
num.methods.adj:setinlined(true)

function num.metamethods.__cast(from, to, exp)
	if from:isfloat() and to == num then
		return `num { DualNumBase.new(exp, nil) }
	else
		error(string.format("ad.t: Cannot cast '%s' to '%s'", tostring(from), tostring(to)))
	end
end


-- =============== FUNCTION GENERATION ===============


local function getvalue(terraquote)
	assert(terraquote.tree.expression.value)
	return terraquote.tree.expression.value
end

-- Boxing up values so that they must be 'touched' before they can
--    be used.
-- Records whether or not it has been touched.
local usedargtable = nil
local Untouched = S.memoize(function(T)
	local struct UntouchedT { value: T }
	UntouchedT.metamethods.__apply = macro(function(self)
		usedargtable[getvalue(self)] = true
		return `self.value
	end)
	UntouchedT.ValueType = T
	return UntouchedT
end)

-- Can be called on any type (primarily num, but possibly others)
--    that defines a 'val' method.
-- Extracts the primal value 
local val = macro(function(x)
	if x:gettype():getmethod("val") then
		return `x:val()
	else
		return x
	end
end)


-- Extract the adjoint from a variable
-- (A no-op if the variable is actually a constant)
local adj = macro(function(v)
	if v:gettype() == num then
		return `v.impl.adj
	else
		return 0.0
	end
end)

-- Set the adjoint of a particular variable
-- (Performs a no-op if the variable is actually a constant)
local setadj = macro(function(v, adjval)
	if v:gettype() == num then
		return quote
			v.impl.adj = adjval
		end
	else
		return quote end
	end
end)

-- Additively accumulate into the adjoint of a particular variable
-- (Performs a no-op if the variable is actually a constant)
local accumadj = macro(function(output, v, adjval)
	return quote
		if adj(output) ~= 0.0 then
			setadj(v, adj(v) + adj(output)*adjval)
		end
	end
end)

local function index(tbl, indices)
	local ret = {}
	for i,index in ipairs(indices) do
		table.insert(ret, tbl[index])
	end
	return ret
end

local function makeADFunction(argTypes, fwdFn, adjFn, usedArgIndices)

	local params = {}
	local paramvals = {}
	for i,t in ipairs(argTypes) do
		local sym = symbol(t)
		table.insert(params, sym)
		if t == num then
			table.insert(paramvals, `sym.impl.val)
		else
			table.insert(paramvals, sym)
		end
	end

	local retfn = nil
	if adjFn then
		local templateTypes = index(argTypes, usedArgIndices)
		local adjUsedParams = index(params, usedArgIndices)
		local DN = DualNum(unpack(templateTypes))
		retfn = terra([params]) : num
			return num { [&DualNumBase](DN.new(fwdFn([paramvals]), adjFn, [adjUsedParams])) }
		end
	else
		retfn = terra([params])
			return fwdFn([paramvals])
		end
	end
	-- These functions are supposed to be (ideally) no slower than their cmath
	-- equivalents, so we always inline them.
	retfn:setinlined(true)
	return retfn
end

-- Last arg is optional, specifies how to 'group' args into blocks of the same type
local function makeOverloadedADFunction(numArgTypes, fwdFn, adjFnTemplate, compsPerType)
	local overallfn = nil
	local numVariants = 2 ^ numArgTypes
	local bitstring = 1
	for i=1,numVariants-1 do
		local types = {}
		for j=0,numArgTypes-1 do
			local typ = nil
			if bit.band(bit.tobit(2^j), bit.tobit(bitstring)) == 0 then
				typ = double
			else
				typ = num
			end
			table.insert(types, typ)
		end
		-- Create multiple instances of each arg type, if called for
		if compsPerType then
			local duptypes = {}
			for i,t in ipairs(types) do
				local numdups = compsPerType[i]
				for j=1,numdups do table.insert(duptypes, t) end
			end
			types = duptypes
		end
		local adjArgTypes = {}
		for _,t in ipairs(types) do table.insert(adjArgTypes, Untouched(t)) end
		local adjFn, usedargindices = nil, nil
		if adjFnTemplate then 
			adjFn, usedargindices = adjFnTemplate(unpack(adjArgTypes))
		end
		local fn = makeADFunction(types, fwdFn, adjFn, usedargindices)
		if not overallfn then
			overallfn = fn
		else
			overallfn:adddefinition(fn:getdefinitions()[1])
		end
		bitstring = bitstring + 1
	end
	overallfn:setinlined(true)
	return overallfn
end

-- Make an adjoint function template
-- When choosing the DualNum type, only templatize on the arguments
--   the are actually used (values or adjoints) in the adjoint function.
--   The indices of these arguments are in the second return value.
local function adjoint(fntemp)
	return function(...)
		local specializedfn = fntemp(...)
		specializedfn:setinlined(true)
		usedargtable = {}
		specializedfn:compile()

		-- Match up used arguments with their types
		local usedtypeindices = {}
		local usedtypes = {}
		local adjfnparams = specializedfn:getdefinitions()[1].typedtree.parameters
		for i,arg in ipairs(adjfnparams) do
			-- Skip arg 1, b/c that's the var itself
			if i ~= 1 then
				if usedargtable[arg.symbol] then
					table.insert(usedtypeindices, i-1)
					table.insert(usedtypes, arg.type.ValueType)
				end
			end
		end	
		local DN = DualNum(unpack(usedtypes))

		-- Construct the list of arguments that will be passed to the adjoint function
		-- These will be either fields on the dual num struct, or dummy values (for arguments
		--    that are unused for this particular specialization of the function). 
		local function makeArgsToAdjFn(dnum)
			local argstoadjfn = {}
			local currFieldIndex = 0
			for i,arg in ipairs(adjfnparams) do
				if i ~= 1 then
					if usedargtable[arg.symbol] then
						local exp = `[arg.type] { dnum.[string.format("_%d", currFieldIndex)] }
						table.insert(argstoadjfn, exp)
						currFieldIndex = currFieldIndex + 1
					else
						table.insert(argstoadjfn, `[arg.type] { 0.0 })
					end
				end
			end
			return argstoadjfn
		end

		-- Wrap the adjoint function (This is the version that will ultimately be called during
		--    gradient computation)
		local wrappedfn = terra(impl: VoidPtr)
			var dnum = [&DN](impl)
			specializedfn(num{[&DualNumBase](dnum)}, [makeArgsToAdjFn(dnum)])
		end

		usedargtable = nil
		return wrappedfn, usedtypeindices
	end
end


-- =============== INSTANTIATE ALL THE FUNCTIONS! ===============

local admath = {}
for k,v in pairs(cmath) do admath[k] = v end

local function addADFunction_simple(name, fn)
	local primalfn = admath[name]
	for i,def in ipairs(fn:getdefinitions()) do
		primalfn:adddefinition(def)
	end
end

local function addADFunction_overloaded(name, numArgs, fwdFn, adjFnTemplate)
	local dualfn = makeOverloadedADFunction(numArgs, fwdFn, adjFnTemplate)
	addADFunction_simple(name, dualfn)
end

local function addADFunction(...)
	local name = (select(1,...))
	-- We silently skip any functions that aren't defined in cmath
	-- (e.g. hyperbolic trig functions don't always exist on Windows)
	if cmath[name] then
		if select("#",...) == 2 then
			addADFunction_simple(...)
		else
			addADFunction_overloaded(...)
		end
	end
end

local function addADOperator(metamethodname, numArgs, fwdFn, adjFnTemplate)
	local fn = makeOverloadedADFunction(numArgs, fwdFn, adjFnTemplate)
	num.metamethods[metamethodname] = fn
end

-- Publically-exposed version of AD primitive creation
-- Last arg is optional, specifies how to 'group' arguments into blocks 
--    of the same type
local function makeADPrimitive(fwdFn, adjFnTemplate, compsPerType)
	-- fwdFn must have only one definition
	assert(#fwdFn:getdefinitions() == 1)
	local typ = fwdFn:gettype()
	-- fwdFn must return only a single value
	assert(#typ.returns == 1)
	-- fwdFn must take only doubles as arguments and return a double as a result
	assert(typ.returns[1] == double)
	for _,t in ipairs(typ.parameters) do assert(t == double) end

	local numArgTypes = compsPerType and #compsPerType or #typ.parameters 
	adjFnTemplate = adjoint(adjFnTemplate)
	local dualfn = makeOverloadedADFunction(numArgTypes, fwdFn, adjFnTemplate, compsPerType)
	dualfn:adddefinition(fwdFn:getdefinitions()[1])
	return dualfn
end

---- Operators ----

-- ADD
addADOperator("__add", 2,
macro(function(a, b) return `a + b end),
adjoint(function(T1, T2)
	return terra(v: num, a: T1, b: T2)
		accumadj(v, a(), 1.0)
		accumadj(v, b(), 1.0)
	end
end))

-- SUB
addADOperator("__sub", 2,
macro(function(a, b) return `a - b end),
adjoint(function(T1, T2)
	return terra(v: num, a: T1, b: T2)
		accumadj(v, a(), 1.0)
		accumadj(v, b(), -1.0)
	end
end))

-- MUL
addADOperator("__mul", 2,
macro(function(a, b) return `a * b end),
adjoint(function(T1, T2)
	return terra(v: num, a: T1, b: T2)
		accumadj(v, a(), val(b()))
		accumadj(v, b(), val(a()))
	end
end))

-- DIV
addADOperator("__div", 2,
macro(function(a, b) return `a / b end),
adjoint(function(T1, T2)
	return terra(v: num, a: T1, b: T2)
		accumadj(v, a(), 1.0/val(b()))
		accumadj(v, b(), -val(a())/(val(b())*val(b())))
	end
end))

-- UNM
addADOperator("__unm", 1,
macro(function(a) return `-a end),
adjoint(function(T)
	return terra(v: num, a: T)
		accumadj(v, a(), -1.0)
	end
end))

-- EQ
addADOperator("__eq", 2,
macro(function(a, b) return `a == b end))

-- LT
addADOperator("__lt", 2,
macro(function(a, b) return `a < b end))

-- LE
addADOperator("__le", 2,
macro(function(a, b) return `a <= b end))

-- GT
addADOperator("__gt", 2,
macro(function(a, b) return `a > b end))

-- GE
addADOperator("__ge", 2,
macro(function(a, b) return `a >= b end))


---- Functions ----

-- ACOS
addADFunction("acos", 1,
cmath.acos,
adjoint(function(T)
	return terra(v: num, a: T)
		accumadj(v, a(), -1.0 / cmath.sqrt(1.0 - (val(a())*val(a()))))
	end
end))

-- ACOSH
addADFunction("acosh", 1,
cmath.acosh,
adjoint(function(T)
	return terra(v: num, a: T)
		accumadj(v, a(), 1.0 / cmath.sqrt((val(a())*val(a())) - 1.0))
	end
end))

-- ASIN
addADFunction("asin", 1,
cmath.asin,
adjoint(function(T)
	return terra(v: num, a: T)
		accumadj(v, a(), 1.0 / cmath.sqrt(1.0 - (val(a())*val(a()))))
	end
end))

-- ASINH
addADFunction("asinh", 1,
cmath.asinh,
adjoint(function(T)
	return terra(v: num, a: T)
		accumadj(v, a(), 1.0 / cmath.sqrt((val(a())*val(a())) + 1.0))
	end
end))

-- ATAN
addADFunction("atan", 1,
cmath.atan,
adjoint(function(T)
	return terra(v: num, a: T)
		accumadj(v, a(), 1.0 / (1.0 + (val(a())*val(a()))))
	end
end))

-- ATAN2
addADFunction("atan2", 2,
cmath.atan2,
adjoint(function(T1, T2)
	return terra(v: num, a: T1, b: T2)
		var sqnorm = (val(a())*val(a())) + (val(b())*val(b()))
		accumadj(v, a(), val(b())/sqnorm)
		accumadj(v, b(), -val(a())/sqnorm)
	end
end))

-- CEIL
addADFunction("ceil",
terra(a: num) : num
	return num(cmath.ceil(a:val()))
end)

-- COS
addADFunction("cos", 1,
cmath.cos,
adjoint(function(T)
	return terra(v: num, a: T)
		accumadj(v, a(), -cmath.sin(val(a())))
	end
end))

-- COSH
addADFunction("cosh", 1,
cmath.cosh,
adjoint(function(T)
	return terra(v: num, a: T)
		accumadj(v, a(), cmath.sinh(val(a())))
	end
end))

-- EXP
addADFunction("exp", 1,
cmath.exp,
adjoint(function(T)
	return terra(v: num, a: T)
		accumadj(v, a(), val(v))
	end
end))

-- FABS
addADFunction("fabs",
terra(a: num)
	if a:val() >= 0.0 then
		return a
	else
		return -a
	end
end)

-- FLOOR
addADFunction("floor",
terra(a: num) : num
	return num(cmath.floor(a:val()))
end)

-- FMAX
local terra fmax(a: num, b: double)
	if a:val() >= b then return a else return num(b) end
end
local terra fmax(a: double, b: num)
	if a > b:val() then return num(a) else return b end
end
local terra fmax(a: num, b: num)
	if a:val() > b:val() then return a else return b end
end
fmax:setinlined(true)
addADFunction("fmax", fmax)

-- FMIN
local terra fmin(a: num, b: double)
	if a:val() <= b then return a else return num(b) end
end
local terra fmin(a: double, b: num)
	if a < b:val() then return num(a) else return b end
end
local terra fmin(a: num, b: num)
	if a:val() < b:val() then return a else return b end
end
fmin:setinlined(true)
addADFunction("fmin", fmin)

-- LOG
addADFunction("log", 1,
cmath.log,
adjoint(function(T)
	return terra(v: num, a: T)
		accumadj(v, a(), 1.0/val(a()))
	end
end))

-- LOG10
addADFunction("log10", 1,
cmath.log10,
adjoint(function(T)
	return terra(v: num, a: T)
		accumadj(v, a(), 1.0/([math.log(10.0)]*val(a())))
	end
end))

-- POW
addADFunction("pow", 2,
cmath.pow,
adjoint(function(T1, T2)
	return terra(v: num, a: T1, b: T2)
		if val(a()) ~= 0.0 then	-- Avoid log(0)
			accumadj(v, a(), val(b())*val(v)/val(a()))
			accumadj(v, b(), cmath.log(val(a()))*val(v))
		end
	end
end))

-- ROUND
addADFunction("round",
terra(a: num)
	return num(cmath.round(a:val()))
end)

-- SIN
addADFunction("sin", 1,
cmath.sin,
adjoint(function(T)
	return terra(v: num, a: T)
		accumadj(v, a(), cmath.cos(val(a())))
	end
end))

-- SINH
addADFunction("sinh", 1,
cmath.sinh,
adjoint(function(T)
	return terra(v: num, a: T)
		accumadj(v, a(), cmath.cosh(val(a())))
	end
end))

-- SQRT
addADFunction("sqrt", 1,
cmath.sqrt,
adjoint(function(T)
	return terra(v: num, a: T)
		accumadj(v, a(), 1.0 / (2.0 * val(v)))
	end
end))

-- TAN
addADFunction("tan", 1,
cmath.tan,
adjoint(function(T)
	return terra(v: num, a: T)
		accumadj(v, a(), 1.0 + val(v)*val(v))
	end
end))

-- TANH
addADFunction("tanh", 1,
cmath.tanh,
adjoint(function(T)
	return terra(v: num, a: T)
		var c = cmath.cosh(val(a()))
		accumadj(v, a(), 1.0 / (c*c))
	end
end))


-- =============== DERIVATIVE COMPUTATION ===============

-- Returns the amount of memory currently being used by the tape
local terra currTapeMemUsed()
	return memPool:currAmountAllocated()
end

-- Returns the maximum amount of memory ever used by the tape
local terra maxTapeMemUsed()
	return memPool:maxAmountAllocated()
end

-- Recover (but do not free) all memory associated with gradient computation
local terra recoverMemory()
	tape:clear()
	memPool:recoverAll()
end

-- Compute the gradient of the given variable w.r.t all other variables
local terra grad(n: num)
	n.impl.adj = 1.0
	for i=0,tape:size() do
		var j = tape:size()-i-1
		tape(j).fn(tape(j).datum)
	end
end

-- Compute the gradient of self w.r.t all other variables
-- Until the next arithmetic op on a num, the adjoints for all other variables
--    will still be correct (though memory has been released for re-use)
terra num:grad(clearTape: bool) : {}
	grad(@self)
	if clearTape then recoverMemory() end
end
terra num:grad() : {}
	self:grad(true)
end

-- Compute the gradient of self w.r.t the given vector of
-- variables and store the result in the given vector of doubles
terra num:grad(indeps: &S.Vector(num), gradient: &S.Vector(double), clearTape: bool) : {}
	grad(@self)
	gradient:clear()
	gradient:reserve(indeps:size())
	for i=0,indeps:size() do
		gradient:insert(indeps(i):adj())
	end
	if clearTape then recoverMemory() end
end
terra num:grad(indeps: &S.Vector(num), gradient: &S.Vector(double)) : {}
	self:grad(indeps, gradient, true)
end


-- =============== EXPORTS ===============

return
{
	num = num,
	math = admath,
	val = val,
	currTapeMemUsed = currTapeMemUsed,
	maxTapeMemUsed = maxTapeMemUsed,
	recoverMemory = recoverMemory
}




