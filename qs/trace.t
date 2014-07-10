local globals = terralib.require("globals")
local util = terralib.require("lib.util")


-- Look up random choice value in the currently-executing trace
-- IMPORTANT: This should record that this program is using this RandomChoice type.
--            Non-POD values should be returned by pointer
local function lookupRandomChoice(RandomChoiceT, args, ctoropts, updateopts)
	-- Just a stub for now
	return quote
		var x : RandomChoiceT.ValueType
	in
		[util.isPOD(RandomChoiceT.ValueType) and (`x) or (`&x)]
	end
end



return 
{
	lookupRandomChoice = lookupRandomChoice,
	exports = 
	{
		--
	}
}


