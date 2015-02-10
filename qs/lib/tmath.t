
-- return terralib.includecstring[[
-- #include <math.h>
-- ]]

-- Now using overloaded AD math primitives instead
local ad = require("qs.lib.ad")
return ad.math
