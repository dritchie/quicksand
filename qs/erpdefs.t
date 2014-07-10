local S = terralib.require("lib.std")
local erp = terralib.require("erp")
local random = terralib.require("random")


local ERPs = {}


ERPs.flip = erp.makeRandomChoice(
	random.bernoulli,
	S.memoize(function(real)
		return terra(currval: bool, p: real) : {bool, real, real}
			if currval then
				return false, real(0.0), real(0.0)
			else
				return true, real(0.0), real(0.0)
			end
		end
	end)
)

ERPs.uniform = erp.makeRandomChoice(
	random.uniform
)

-- TODO:
-- uniform, beta, gamma - when real = num, automatically insert bounds


return 
{
	exports = ERPs
}