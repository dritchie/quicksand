-- This is the main package index file

local util = terralib.require("qs.lib.util")

local qs = util.require("globals")

local function addExports(...)
	local files = {...}
	for _,file in ipairs(files) do
		local mod = util.require(file)
		if mod.exports then
			for k,v in pairs(mod.exports) do
				qs[k] = v
			end
		end
	end
end


addExports(
	"erp",
	"erpdefs",
	"trace",
	"progmodule",
	"infer",
	"mcmc"
)


return qs