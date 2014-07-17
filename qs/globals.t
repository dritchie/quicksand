local util = terralib.require("qs.lib.util")
local S = util.require("lib.std")

-- Globally-available stuff
-- This will form the set of methods/types that get exported
--    as the 'qs' package
local globals = {}


-- Primitive floating point type. Change this for different floating point precision
globals.primfloat = double

-- Dual number type that replaces primfloat during automatic differentiation (AD)
-- TODO: Declare this
globals.dualnum = nil

-- The type of real numbers that a program sees
-- Either globals.primfloat or globals.dualnum
-- Defaults to globals.primfloat
globals.real = globals.primfloat

-- A sample drawn from a probabilistic program.
-- Just bundles a program return value with a log probability (score).
globals.Sample = S.memoize(function(T)
	local struct Sample(S.Object)
	{
		value: T,
		logprob: globals.primfloat
	}
	terra Sample:__init(val: T, lp: globals.primfloat) : {}
		self.value = val
		self.logprob = lp
	end
	terra Sample:__init(val: T) : {}
		self.value = val
		self.logprob = 0.0
	end
	return Sample
end)


return globals

-- Later, globals will get populated with more stuff from other files



