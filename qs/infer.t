local util = terralib.require("qs.lib.util")

local S = util.require("lib.std")
local globals = util.require("globals")
local progmod = util.require("progmodule")
local trace = util.require("trace")



-- Get the sample type for a particular program
local function SampleType(program)
	progmod.assertIsProgram(program, "SampleType")
	local tfn, RetType = program:compile()
	return globals.Sample(RetType)
end


-- 'infer' takes a number of options and returns a Terra function which, when called,
--    performs the requested inference operation.
-- Arguments:
--    * program: A probabilistic program (i.e. a qs.program)
--    * query: A Lua function taking a program and returning a Terra function which
--             performs some operation on a Vector(SampleType(program)).
--    * method: A Lua function taking a program and returning a Terra function which
--              somehow draws samples from that program.
local function infer(program, query, method)
	progmod.assertIsProgram(program, "infer")
	local methodfn = method(program)
	local queryfn = query(program)
	return terra()
		var samples = [S.Vector(SampleType(program))].salloc():init()
		-- Populate sample vector with samples
		methodfn(samples)
		-- Process samples and return result to caller
		return queryfn(samples)
	end
end



--------------------------------------------
--      Super simple inference methods    --
--------------------------------------------


-- Forward sampling is the simplest inference method
-- (Just run program forward repeatedly)
-- NOTE: scores (logprobs) of samples returned from this method are all 0
local function ForwardSample(numsamps)
	return function(program)
		progmod.assertIsProgram(program, "ForwardSample")
		local tfn = program:compile()
		return terra(samples: &S.Vector(SampleType(program)))
			for i=0,numsamps do
				samples:insert()
				var s = samples:get(samples:size()-1)
				s.value = tfn()
				s.logprob = 0.0
			end
		end
	end
end


-- Rejection sampling does a little bit more
-- (Repeatedly run trace rejection initialization)
local function WeightedRejectionSample(numsamps)
	return function(program)
		progmod.assertIsProgram(program, "WeightedRejectionSample")
		local TraceType = trace.RandExecTrace(program, globals.primfloat)
		return terra(samples: &S.Vector(SampleType(program)))
			for i=0,numsamps do
				var tr = TraceType.salloc():init()
				samples:insert()
				var s = samples:get(samples:size()-1)
				S.copy(s.value, tr.returnValue)
				s.logprob = tr.logprob
			end
		end
	end
end



-----------------------------------------
--      Some commonly-used queries     --
-----------------------------------------


-- Just return the raw samples
-- IMPORTANT: Caller is responsible for the memory of the returned vector.
function Samples(program)
	progmod.assertIsProgram(program, "Samples")
	return terra(samples: &S.Vector(SampleType(program)))
		-- Need to copy, since we don't have ownership of 'samples'
		var retsamps : [S.Vector(SampleType(program))]
		retsamps:copy(samples)
		return retsamps
	end
end

-- Return the mean (and optionally the variance) of the samples
-- Requres that the return type of program have the following defined:
--    * The "+" operator
--    * The "-" operator
--    * The "*" operator (as an inner product)
--    * The scalar "/" operator
-- IMPORTANT: Caller is responsible for the memory of the returned mean object.
function Expectation(doVariance)
	return function(program)
		progmod.assertIsProgram(program, "Expectation")
		local tfn, RetType = program:compile()
		-- If program's return type is bool or int, then automatically convert to
		--    a floating-point type for representing real number averages.
		local AccumType = (RetType:isintegral() or RetType:islogical()) and globals.primfloat or RetType
		return terra(samples: &S.Vector(SampleType(program)))
			S.assert(samples:size() > 0)
			var m = AccumType(samples(0))
			for s in samples do
				var _m = m
				m = m + s.value
				S.rundestructor(_m)
			end
			var _m = m
			m = m / samples:size()
			S.rundestructor(_m)
			escape
				if doVariance then
					emit quote
						var v : globals.primfloat = 0.0
						for s in samples do
							var diff = s.value - m
							v = v + diff*diff
							S.rundestructor(diff)
						end
						v = v / samples:size()
						return m, v
					end
				else
					emit quote return m end
				end
			end
		end
	end
end

-- Return the MAP estimate from a list of samples
-- IMPORTANT: Caller is responsible for the memory of the returned object.
function MAP(program)
	progmod.assertIsProgram(program, "MAP")
	return terra(samples: &S.Vector(SampleType(program)))
		var bestVal : &SampleType(program)
		var bestScore : globals.primfloat = [-math.huge]
		for s in samples do
			if s.logprob > bestScore then
				bestScore = s.logprob
				bestVal = &s.value
			end
		end
		var retval : SampleType(program)
		retval:copy(bestVal)
		return retval
	end
end

-- Return the autocorrelation of the samples at every time lag.
-- Optionally takes a 'true' mean and variance to use in its calculations
--    (if not specified, it will compute the sample mean and variance)
-- 'mean' and 'variance' can be Lua constants or Terra quotes
-- IMPORTANT: Caller is responsible for the memory of the returned vector.
function Autocorrelation(mean, variance)
	assert((mean or variance) and (mean and variance),
		"Autocorrelation: both mean and variance must be provided if one is provided.")
	return function(program)
		progmod.assertIsProgram(program, "MAP")

		local terra withMeanAndVar(samples: &S.Vector(SampleType(program)),
								   m: SampleType(program), v: globals.primfloat)
			var ac : S.Vector(globals.primfloat)
			ac:init()
			for t=0,samples:size() do
				var act = globals.primfloat(0.0)
				var n = samples:size() - t
				for i=0,n do
					var tmp1 = samples(i).value - m
					var tmp2 = samples(i+t) - m
					act = act + tmp1*tmp2
					S.rundestructor(tmp1)
					S.rundestructor(tmp2)
				end
				if n > 0 then
					act = act / (n * v)
				end
				ac:insert(act)
			end
			return ac
		end

		local terra noMeanAndVar(samples: &S.Vector(SampleType(program)))
			var m, v = [Expectation(true)(program)](samples)
			return withMeanAndVar(samples, m, v)
		end

		if not mean then
			return noMeanAndVar
		else
			-- If mean/variance are quotes, use a macro (callable from Terra)
			-- If they are constants, use a Terra function (callable from Lua)
			if terralib.isquote(mean) then
				return macro(function(samples)
					return `withMeanAndVar(samples, mean, variance)
				end)
			else
				return terra(samples: &S.Vector(SampleType(program)))
					return withMeanAndVar(samples, mean, variance)
				end
			end
		end
	end
end



return
{
	exports = 
	{
		SampleType = SampleType,
		infer = infer,
		ForwardSample = ForwardSample,
		WeightedRejectionSample = WeightedRejectionSample,
		Samples = Samples,
		Expectation = Expectation,
		MAP = MAP,
		Autocorrelation = Autocorrelation
	}
}




