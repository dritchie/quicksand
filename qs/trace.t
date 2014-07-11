local globals = terralib.require("globals")
local util = terralib.require("lib.util")


-- Look up random choice value in the currently-executing trace
-- (Does address stack management...)
-- IMPORTANT: This should record that this program is using this RandomChoice type.
--            Non-POD values should be returned by pointer
local function lookupRandomChoice(RandomChoiceT, args, ctoropts, updateopts)
	-- Just a test stub for now
	RandomChoiceT.methods.__init:compile()
	RandomChoiceT.methods.update:compile()
	RandomChoiceT.methods.rescore:compile()
	RandomChoiceT.methods.proposal:compile()
	return quote
		var rc : RandomChoiceT
		rc:init([args], [ctoropts])
		-- rc:update([args], [updateopts])
		var val = rc:getValue()
	in
		[util.isPOD(RandomChoiceT.ValueType) and (`val) or (`&val)]
	end
end

-- Just a stub for now
local factor = macro(function(num)
	return quote
		var x = num
	end
end)


return 
{
	lookupRandomChoice = lookupRandomChoice,
	factor = factor,
	exports = 
	{
		factor = factor
	}
}


