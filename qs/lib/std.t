local S = {}

S.memoize = terralib.memoize

-- for speed we should just declare the methods we need directly using
-- terra, but we need an API to do this
local C = terralib.includecstring [[
#include <stdio.h>
#include <stdlib.h>
]]

S.rundestructor = macro(function(self)
    local T = self:gettype()
    local function hasdtor(T) --avoid generating code for empty array destructors
        if T:isstruct() then return T:getmethod("destruct") 
        elseif T:isarray() then return hasdtor(T.type) 
        else return false end
    end
    if T:isstruct() then
        local d = T:getmethod("destruct")
        if d then
            return `self:destruct()
        end
    elseif T:isarray() and hasdtor(T) then        
        return quote
            var pa = &self
            for i = 0,T.N do
                S.rundestructor((@pa)[i])
            end
        end
    end
    return quote end
end)

S.assert = macro(function(check)
    local loc = check.tree.filename..":"..check.tree.linenumber
    return quote 
        if not check then
            C.printf("%s: assertion failed!\n",loc)
            C.abort()
        end
    end
end) 

local generatedtor = macro(function(self)
    local T = self:gettype()
    local stmts = terralib.newlist()
    -- First, execute any custom destructor logic
    if T.methods.__destruct then
        stmts:insert(`self:__destruct())
    end
    -- Then, no matter what, destruct all the members of this struct
    -- The default behavior is overridable by the "__destructmembers" method
    -- This can be useful for e.g. inheritance systems, where an object's
    --    dynamic type can differ from its static type.
    if T.methods.__destructmembers then
        stmts:insert(`self:__destructmembers())
    else
        local entries = T:getentries()
        for i,e in ipairs(entries) do
            if e.field then --not a union
                stmts:insert(`S.rundestructor(self.[e.field]))
            end
        end
    end
    return stmts
end)



S.init = macro(function(self)
    local T = self:gettype()
    local function hasinit(T)
        if T:isstruct() then return T:getmethod("init")
        elseif T:isarray() then return hasinit(T.type)
        else return false end
    end
    if T:isstruct() and hasinit(T) then
        return `self:init()
    elseif T:isarray() and hasinit(T) then
        return quote
            var pa = &self
            for i=0,T.N do
                S.init((@pa)[i])
            end
        end
    end
    return quote end
end)

S.initmembers = macro(function(self)
    local T = self:gettype()
    local entries = T:getentries()
    return quote
        escape
            for _,e in ipairs(entries) do
                if e.field then --not a union
                    emit `S.init(self.[e.field])
                end
            end
        end
    end
end)

local generateinit = macro(function(self, ...)
    local T = self:gettype()
    local args = {...}
    return quote
        escape
            if T.methods.__init then
                emit `self:__init([args])
            else
                emit `S.initmembers(self)
            end
        end
    end
end)



S.copy = macro(function(self, other)
    local T = self:gettype()
    local function hascopy(T)
        if T:isstruct() then return T:getmethod("copy")
        elseif T:isarray() then return hascopy(T.type)
        else return false end
    end
    if T:isstruct() and hascopy(T) then
        return `self:copy(&other)
    elseif T:isarray() and hascopy(T) then
        return quote
            var pa = &self
            for i=0,T.N do
                S.copy((@pa)[i], other[i])
            end
        end
    end
    local To = other:gettype()
    return quote
        self = other
    end
end)

S.copymembers = macro(function(self, other)
    local T = self:gettype()
    local entries = T:getentries()
    return quote
        escape
            for _,e in ipairs(entries) do
                if e.field then --not a union
                    emit `S.copy(self.[e.field], other.[e.field])
                end
            end
        end
    end
end)

local generatecopy = macro(function(self, other)
    local T = self:gettype()
    return quote
        escape
            if T.methods.__copy then
                emit `self:__copy(&other)
            else
                emit `S.copymembers(self, other)
            end
        end
    end
end)



-- standard object metatype
-- provides T.alloc(), T.salloc(), obj:destruct(), obj:delete()
-- users should define __destruct if the object has custom destruct behavior
-- destruct will call destruct on child nodes
function S.Object(T)
    --fill in special methods/macros
    terra T:delete() : {}
        self:destruct()
        C.free(self)
    end 
    terra T.methods.alloc() : &T
        return [&T](C.malloc(sizeof(T)))
    end
    T.methods.salloc = macro(function()
        return quote
            var t : T
            defer t:destruct()
        in
            &t
        end
    end)
    terra T:destruct() : {}
        generatedtor(@self)
    end
    T.methods.init = macro(function(self, ...)
        local args = {...}
        return quote
            var s = &self
            generateinit(@s, [args])
        in
            s
        end
    end)
    terra T:initmembers() : {}
        S.initmembers(@self)
    end
    terra T:copy(other: &T) : &T
        generatecopy(@self, @other)
        return self
    end
    terra T:copymembers(other: &T)
        S.copymembers(@self, @other)
    end
end


function S.Vector(T,debug)
    local struct Vector(S.Object) {
        _data : &T;
        _size : uint64;
        _capacity : uint64;
    }
    function Vector.metamethods.__typename() return ("Vector(%s)"):format(tostring(T)) end
    Vector.isstdvector = true
    Vector.type = T
    local assert = debug and S.assert or macro(function() return quote end end)
    terra Vector:__init() : {}
        self._data,self._size,self._capacity = nil,0,0
    end
    terra Vector:__init(cap : uint64) : {}
        self:__init()
        self:reserve(cap)
    end
    terra Vector:reserve(cap : uint64)
        if cap > 0 and cap > self._capacity then
            var oc = self._capacity
            if self._capacity == 0 then
                self._capacity = 16
            end
            while self._capacity < cap do
                self._capacity = self._capacity * 2
            end
            self._data = [&T](S.realloc(self._data,sizeof(T)*self._capacity))
        end
    end
    terra Vector:resize(size: uint64)
        self:reserve(size)
        self._size = size
    end
    terra Vector:__destruct()
        self:clear()
        if self._data ~= nil then
            C.free(self._data)
            self._data = nil
            self._capacity = 0
        end
    end
    terra Vector:size() return self._size end
    
    terra Vector:get(i : uint64)
        assert(i < self._size) 
        return &self._data[i]
    end
    Vector.metamethods.__apply = macro(function(self,idx)
        return `@self:get(idx)
    end)
    
    terra Vector:insert(idx : uint64, N : uint64, v : T) : {}
        assert(idx <= self._size)
        self._size = self._size + N
        self:reserve(self._size)
        
        if self._size > N then
            var i = self._size
            while i > idx do
                self._data[i - 1] = self._data[i - 1 - N]
                i = i - 1
            end
        end
        
        for i = 0ULL,N do
            self._data[idx + i] = v
        end
    end
    terra Vector:insert(idx : uint64, v : T) : {}
        return self:insert(idx,1,v)
    end
    terra Vector:insert(v : T) : {}
        return self:insert(self._size,1,v)
    end
    terra Vector:insert() : &T
        self._size = self._size + 1
        self:reserve(self._size)
        return self:get(self._size - 1)
    end
    terra Vector:remove(idx : uint64) : T
        assert(idx < self._size)
        var v = self._data[idx]
        self._size = self._size - 1
        for i = idx,self._size do
            self._data[i] = self._data[i + 1]
        end
        return v
    end
    terra Vector:remove() : T
        assert(self._size > 0)
        return self:remove(self._size - 1)
    end

    terra Vector:clear() : {}
        assert(self._capacity >= self._size)
        for i = 0ULL,self._size do
            S.rundestructor(self._data[i])
        end
        self._size = 0
    end

    terra Vector:__copy(other: &Vector) : {}
        self:__init(other:size())
        for i=0,other:size() do
            self:insert()
            S.copy(self(i), other(i))
        end
    end

    Vector.metamethods.__eq = terra(self: Vector, other: Vector) : bool
        if self:size() ~= other:size() then return false end
        for i=0,self:size() do
            if not (self(i) == other(i)) then return false end
        end
        return true
    end
    Vector.metamethods.__eq:setinlined(true)

    Vector.metamethods.__for = function(syms, iter, body)
        local e = symbol()
        return {`@e}, quote
            var self = iter
            for i=0,self:size() do
                var [e] = self:get(i)
                body
            end
        end
    end
    
    return Vector
end

S.Vector = S.memoize(S.Vector)

--import common C functions into std object table
for k,v in pairs(C) do
    S[k] = v
end

return S