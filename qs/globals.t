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


return globals

-- Later, globals will get populated with more stuff from other files



