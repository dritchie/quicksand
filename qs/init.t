-- This is the main package index file

local qs = terralib.require("globals")

local function addExports(...)
	local files = {...}
	for _,file in ipairs(files) do
		local mod = terralib.require(file)
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
	"progmodule"
	-- TODO: add more files
)


return qs