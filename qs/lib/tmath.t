
-- return terralib.includecstring[[
-- #include <math.h>
-- ]]

-- Now using overloaded AD math primitives instead
local util = terralib.require("qs.lib.util")
local ad = util.require("ad")
return ad.math