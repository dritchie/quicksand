
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


return U




