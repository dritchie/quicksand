
-- Globally-available stuff
-- This will form the set of methods/types that get exported
--    as the 'quicksand' package
local qs = {}


-- The type of real numbers
-- Defaults to double, but may be changed by the backend
--    to other types, specifically an AD dual number type.
qs.real = double

-- Flags/fields used internally by the backend and not
--    intended for use by client code.
qs.__compiler = {
	
	-- The program currently being compiled
	currentProgram = nil,

	-- Set to true during a trace update
	isRecordingTrace = global(bool, false),

	-- Set to false during a trace update where control flow structure
	--    is known to be fixed. 
	canStructureChange = global(bool, true)

}


-- Other files will include this one and populate qs with more contents


return qs



