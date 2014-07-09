local S = terralib.require("lib.std")
local tmath = terralib.require("lib.math")
local qs = terralib.require("globals")


-- If a sampling function has unit return type, then we assume
--    the sample value is passed out in the last argument
--    (which must be a pointer type)
-- If true, the second return value is the (dereferenced) type of the out argument
local function samplingFunctionReturnsByArgument(sl)
	local sampleFnType = sl.sample:gettype()
	local sampleHasUnitType = sampleFnType.returntype == tuple()
	local lastArgType = sampleFnType.params[#sampleFnType.params]
	local sampleReturnedByArg = sampleHasUnitType and lastArgType:ispointer()
	if sampleHasUnitType and not sampleReturnedByArg then
		error("qs.makeRandomChoice - Sampling function with unit return type should return by its last argument.")
	end
	local RetType = sampleReturnedByArg and lastArgType.type or nil
	return sampleReturnedByArg, RetType
end

-- The number of parameters required by a random choice
local function getNumParams(sl)
	-- The number of arguments to logprob, minus 1 (that's the value to be scored)
	return #sl.logprob:gettype().parameters - 1
end

-- Default proposal for when no custom one is provided
-- (Just resamples from the prior)
local function makeDefaultProposal(sl)
	return macro(function(currval, ...)
		local args = {...}
		local sampleReturnedByArg, RetType = samplingFunctionReturnsByArgument(sl)
		if sampleReturnedByArg then
			return quote
				var newval : RetType
				S.init(newval)
				sl.sample([args], &newval)
				var fwdPropLP = sl.logprob(&newval, [args])
				var rvsPropLP = sl.logprob(&currval, [args])
			in
				newval, fwdPropLP, rvsPropLP
			end
		else
			return quote
				var newval = sl.sample([args])
				var fwdPropLP = sl.logprob(newval, [args])
				var rvsPropLP = sl.logprob(currval, [args])
			in
				newval, fwdPropLP, rvsPropLP
			end
		end
	end)
end



-- Random choices can be passed an optional struct with extra arguments
-- The following functions assume that the struct is passed in as a 
--    typed quote.
local function structHasMember(s, name)
	local t = s:gettype()
	for _,e in ipairs(t.entries) do
		if e.efield == name then
			return true
		end
	end
	return false
end



-- Scalar-valued random choices can be subject to lower and upper bounds
local BoundState = {
	None = 0,
	Lower = 1,
	Upper = 2,
	LowerUpper = 3
}
-- The bounding state of a random choice is determined by arguments
--    passed in the optional argument struct.
local function getBoundState(opts)
	if not opts then
		return BoundState.None
	end
	local haslo = structHasMember(opts, "lo")
	local hashi = structHasMember(opts, "hi")
	if haslo and hashi then
		return BoundState.LowerUpper
	elseif haslo then
		return BoundState.Lower
	elseif hashi then
		return BoundState.Upper
	else
		return BoundState.None
	end
end
-- We implement bounding via variable transformations
local logistic = macro(function(x)
	return `1.0 / (1.0 + ad.math.exp(-x))
end)
local invlogistic = macro(function(y)
	return `-ad.math.log(1.0/y - 1.0)
end)
local function getBoundingTransforms(boundState)
	local forward = nil
	local inverse = nil
	local priorincr = nil
	if boundState == BoundState.None then
		forward = function(x, obj) return x end
		inverse = function(y, obj) return y end
		priorincr = function(x, obj) return `0.0 end
	elseif boundState == BoundState.Lower then
		forward = function(x, obj) return `tmath.exp(x) + obj.lo end
		inverse = function(y, obj)
			local z = `tmath.fmax(y, obj.lo + 1e-15)
			return `tmath.log(z - obj.lo)
		end
		priorincr = function(x, obj) return x end
	elseif boundState == BoundState.Upper then
		forward = function(x, obj) return `obj.hi - tmath.exp(x) end
		inverse = function(y, obj)
			local z = `tmath.fmin(y, obj.hi - 1e-15)
			return `tmath.log(obj.hi - z)
		end
		priorincr = function(x, obj) return x end
	elseif boundState == BoundState.LowerUpper then
		foward = function(x, obj)
			return quote
				var logit = logistic(x)
				if x > [-math.huge] and logit == 0.0 then logit = 1e-15 end
				if x < [math.huge] and logit == 1.0 then logit = [1.0 - 1e-15] end
				var y = obj.lo + (obj.hi - obj.lo) * logit
			in
				y
			end
		end
		inverse = function(y, obj)
			local z = `tmath.fmax(tmath.fmin(y, obj.hi - 1e-15), obj.lo + 1e-15)
			local t = `(z - obj.lo) / (obj.hi - obj.lo)
			return `invlogistic(t)
		end
		priorincr = function(x, obj)
			return `tmath.log(obj.hi - obj.lo) - x - 2.0*tmath.log(1.0 + tmath.exp(-x))
		end
	else
		error("getBoundingTransforms - Unknown bounding state")
	end
	return forward, inverse, priorincr
end



function qs.makeRandomChoice(sampleAndLogprob, propose)

	-- The RandomChoice struct records the parameters and value of the random choice
	--    in the execution trace.
	local RandomChoice = S.memoize(function(real, boundState)
		local sl = sampleAndLogprob(real)
		propose = propose or makeDefaultProposal(sl)
		local struct RandomChoiceT(S.Object)
		{
			-- Deal with params that are pointer-to-struct...
			-- Special logic for vector params(?)
		}
		return RandomChoiceT
	end)

	-- This macro is how the random choice is exposed to client code
	local erp = macro(function(...)
		local args = {...}
		local sl = sampleAndLogprob(qs.real)
		local n = getNumParams(sl)
		local N = #args
		assert(N == n or N == n+1, "erp.observe - Too many parameters")
		local opts = nil
		if N == n+1 then
			local opts = args[N]
			args[N] = nil
		end
		local bs = getBoundState(opts)
		local fwd, inv, lpincr = getBoundingTransforms(bs)
		local RandomChoiceT = RandomChoice(qs.real, bs)
		return quote
			if qs.isRecordingTrace then
				--
			else
				-- Just draw a sample (respect bounds!)
			end
		end
	end)

	-- This macro facilitates models where we directly observe the values
	--    of some random choices, instead of sampling them
	erp.observe = macro(function(value, ...)
		local args = {...}
		local sl = sampleAndLogprob(qs.real)
		local n = getNumParams(sl)
		local N = #args
		assert(N == n or N == n+1, "erp.observe - Too many parameters")
		local opts = nil
		if N == n+1 then
			local opts = args[N]
			args[N] = nil
		end
		local fwd, inv, lpincr = getBoundingTransforms(getBoundState(opts))
		return quote
			qs.factor(sl.logprob(value, [args]) + lpincr(value, opts))
		end
	end)

end


-- TODO:
-- Uniform, beta, gamma - when real = num, automatically insert bounds





