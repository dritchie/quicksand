local U = {}

function U.includec_path(filename)
	local cpath = os.getenv("C_INCLUDE_PATH") or "."
	return terralib.includec(filename, "-I", cpath)
end

-- Cross platform
U.fatalError = macro(function(...)
	local args = {...}
	return quote
		C.printf("[Fatal Error] ")
		C.printf([args])
		-- Traceback only supported on POSIX systems
		[U.isPosix() and quote terralib.traceback(nil) end or quote end]
		C.exit(1)
	end
end)

-- Is a type "plain old data," according to Standard Object conventions?
-- Used in some places to determine when something should be passed by value or by pointer
-- (POD objects pass by value, non-POD objects pass by reference)
function isPOD(typ)
	-- Non-struct types are fine
	if not typ:isstruct() then return true end
	-- User-defined ctors, dtors, or copiers are a no-no
	if typ:getmethod("__init") or typ:getmethod("__destruct") or type:getmethod("__copy") then
		return false
	end
	-- Also can't have any members that are non-POD
	for _,e in ipairs(typ.entries) do
		if not isPOD(e.type) then return false end
	end
	return true
end
U.isPOD = isPOD

return U




