local S = terralib.require("qs.lib.std")
local rand = terralib.require("qs.lib.random")

-- Globally-available stuff
-- This will form the set of methods/types that get exported
--    as the 'qs' package
local qs = {}


-- Primitive floating point type.
qs.float = double


-- The type of real numbers that a program sees
-- Defaults to qs.float
qs.real = qs.float

-- Expose the function that seeds the random number generator
qs.initrand = rand.initrand

return qs

-- Later, qs will get populated with more stuff from other files



