local U = {}

function U.includec_path(filename)
	local cpath = os.getenv("C_INCLUDE_PATH") or "."
	return terralib.includec(filename, "-I", cpath)
end

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

-- Equality comparison that also handles arrays
local equal
equal = macro(function(a, b)
	local A = a:gettype()
	local B = b:gettype()
	if A:isarray() or B:isarray() then
		if A ~= B then return false end
		local expr = `equal(a[0], b[0])
		for i=1,A.N-1 do expr = (`expr and equal(a[ [i] ], b[ [i] ])) end
		return expr
	end
	return `a == b
end)
U.equal = equal

local function stringsplit(self, sep)
    local sep, fields = sep or ":", {}
    local pattern = string.format("([^%s]+)", sep)
    self:gsub(pattern, function(c) fields[#fields+1] = c end)
    return fields
end
function U.require(name)
	local callermodule = debug.getinfo(2, "S").source:gsub("@", ""):gsub("%.t", "")
	local parts = stringsplit(callermodule, "/")
	local path = name
	if #parts > 1 then
		path = parts[1]
		for i=2,#parts-1 do path = string.format("%s.%s", path, parts[i]) end
		path = string.format("%s.%s", path, name)
	end
	return terralib.require(path)
end

return U




