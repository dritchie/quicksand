local util = terralib.require("qs.lib.util")
local S = util.require("lib.std")
local ad = util.require("lib.ad")
local rand = util.require("lib.random")

-- Globally-available stuff
-- This will form the set of methods/types that get exported
--    as the 'qs' package
local qs = {}


-- Primitive floating point type.
qs.float = double

-- Dual number type that replaces float during automatic differentiation (AD)
qs.dualnum = ad.num

-- The type of real numbers that a program sees
-- Either qs.float or qs.dualnum
-- Defaults to qs.float
qs.real = qs.float

-- Retrieves the value of a number, whether it's primitive or a dual number
qs.val = ad.val

-- Expose the function that seeds the random number generator
-- TODO: Should this be here?
qs.initrand = rand.initrand

return qs

-- Later, qs will get populated with more stuff from other files



