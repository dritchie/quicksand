local globals = terralib.require("globals")
local util = terralib.require("lib.util")



-- The program currently being compiled
local currentProgram = nil

-- Set to true during a trace update
local isRecordingTrace = global(bool, false)

-- Set to false during a trace update where control flow structure
--    is known to be fixed. 
local canStructureChange = global(bool, true)




-- Look up random choice value in the currently-executing trace
-- (Does address stack management...)
-- IMPORTANT: This should record that this program is using this RandomChoice type.
--            Non-POD values should be returned by pointer
local function lookupRandomChoice(RandomChoiceT, args, ctoropts, updateopts)
	-- TODO: Actually implement this.
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

local factor = macro(function(num)
	-- TODO: Actually implement this.
	return quote
		var x = num
	end
end)


return 
{
	currentProgram = currentProgram,
	isRecordingTrace = isRecordingTrace,
	lookupRandomChoice = lookupRandomChoice,
	factor = factor,
	exports = 
	{
		factor = factor
	}
}


