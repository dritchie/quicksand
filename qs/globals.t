
-- Globally-available stuff
-- This will form the set of methods/types that get exported
--    as the 'quicksand' package
local globals = {}


-- Primitive floating point type. Change this for different floating point precision
globals.primfloat = double

-- Dual number type that replaces primfloat during automatic differentiation (AD)
-- TODO: Declare this
globals.dualnum = nil

-- The type of real numbers that a program sees
-- Either globals.primfloat or globals.dualnum
-- Defaults to globals.primfloat
globals.real = globals.primfloat

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



