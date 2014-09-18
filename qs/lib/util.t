local U = {}


function U.includec_path(filename)
	local cpath = os.getenv("C_INCLUDE_PATH") or "."
	return terralib.includec(filename, "-I", cpath)
end


-- Is a type "plain old data," according to Standard Object conventions?
-- Used in some places to determine when something should be passed by value or by pointer
-- (POD objects pass by value, non-POD objects pass by pointer)
function isPOD(typ)
	-- Non-struct types are fine
	if not typ:isstruct() then return true end
	-- User-defined ctors, dtors, or copiers are a no-no
	if typ:getmethod("__init") or typ:getmethod("__destruct") or typ:getmethod("__copy") then
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


-- Timing
local timestuff = terralib.includecstring [[
#include <stdlib.h>

#ifndef _WIN32
#include <sys/time.h>
double __currentTimeInSeconds() {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return tv.tv_sec + tv.tv_usec / 1000000.0;
}
#else
#include <time.h>
double __currentTimeInSeconds() {
	return time(NULL);
}
#endif
]]
U.currentTimeInSeconds = timestuff.__currentTimeInSeconds


U.swap = macro(function(a, b)
	return quote
		var tmp = a
		a = b
		b = tmp
	end
end)


return U




