local util = require("qs.lib.util")

local thisfile = debug.getinfo(1, "S").source:gsub("@", "")
local fasthash = util.includec_path(thisfile:gsub("hash.t", "fasthash.h")).SuperFastHash
local S = require("qs.lib.std")

local C = terralib.includecstring [[
#include <string.h>
]]

----------------------------------------------------------
-- TODO: Eventually replace all this with Zach's hashtable
----------------------------------------------------------

local Hash = {}

-- We can provide a 'default' hash for aggregates that can be
--    easily adopted but is not present unless explicity asked for.
local function getDefaultHash(typ)
	local fn = terra(val: typ)
		return fasthash([&int8](&val), sizeof(typ))
	end
	fn:setinlined(true)
	return fn
end

-- Expose the default hash function
Hash.hash = S.memoize(function(T) return getDefaultHash(T) end)

-- Also expose the raw hashing function, in case it's needed
Hash.rawhash = fasthash

-- Also provide mechanism for users to provide their own
--    hashing behavior with the __hash method
Hash.gethashfn = function(typ)
	if typ:isprimitive() or typ:ispointer() then
		return Hash.hash(typ)
	elseif typ:isstruct() and typ:getmethod("__hash") then
		return macro(function(val) return `val:__hash() end)
	else
		error(string.format("No __hash method for aggregate type '%s'", tostring(K)))
	end
end

-- Simple wrapper around rawstring that allows for hashing
local struct HashableString { str: rawstring }
terra HashableString:__hash()
	return fasthash(self.str, C.strlen(self.str))
end
HashableString.metamethods.__eq = terra(hs1: HashableString, hs2: HashableString)
	return C.strcmp(hs1.str, hs2.str) == 0
end
Hash.HashableString = HashableString

-- Export all this stuff as a module
return Hash
