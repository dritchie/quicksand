
-- Globally-available stuff
-- This will form the set of methods/types that get exported
--    as the 'quicksand' package
local globals = {}


-- The type of real numbers
-- Defaults to double, but may be changed by the backend
--    to other types, specifically an AD dual number type.
globals.real = double

-- Flags/fields used internally by the backend and not
--    intended for use by client code.
globals.__compiler = {
	
	-- The program currently being compiled
	currentProgram = nil,

	-- Set to true during a trace update
	isRecordingTrace = global(bool, false),

	-- Set to false during a trace update where control flow structure
	--    is known to be fixed. 
	canStructureChange = global(bool, true)

}

return globals

-- Later, globals will get populated with more stuff from other files



