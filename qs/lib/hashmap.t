local util = terralib.require("qs.lib.util")

local hash = terralib.require("qs.lib.hash")
local S = terralib.require("qs.lib.std")


local defaultInitialCapacity = 8
local expandFactor = 2
local loadFactor = 2.0


-- NOTE: In all use cases, we assume that:
--    * Keys should be copied if they are ever to be stored (since whether a key is found
--      or created/stored is unpredictable/unknowable for some methods).
--    * Values should not be copied (i.e. the user is handing off
--       ownership of that memory to us).


local HM = S.memoize(function(K, V, hashfn)

	hashfn = hashfn or hash.gethashfn(K)

	local struct HashCell(S.Object)
	{
		key: K,
		val: V,
		next: &HashCell
	}

	terra HashCell:__init(k: K, v: V)
		S.copy(self.key, k)
		self.val = v
		self.next = nil
	end

	terra HashCell:__init(k: K)
		S.copy(self.key, k)
		S.init(self.val)
		self.next = nil
	end

	terra HashCell:__destruct() : {}
		if self.next ~= nil then
			self.next:delete()
		end
	end

	-----

	local struct HashMap(S.Object)
	{
		__cells: &&HashCell,
		__capacity: uint,
		__size: uint
	}
	function HashMap.metamethods.__typename() return ("HashMap(%s,%s)"):format(tostring(K), tostring(V)) end

	terra HashMap:__init(initialCapacity: uint) : {}
		self.__capacity = initialCapacity
		self.__cells = [&&HashCell](S.malloc(initialCapacity*sizeof([&HashCell])))
		for i=0,self.__capacity do
			self.__cells[i] = nil
		end
		self.__size = 0
	end

	terra HashMap:__init() : {}
		self:__init(defaultInitialCapacity)
	end

	terra HashMap:__copy(other: &HashMap)
		self:__init(other.__capacity)
		for i=0,self.__capacity do
			if other.__cells[i] ~= nil then
				var cell = HashCell.alloc():copy(other.__cells[i])
				self.__cells[i] = cell
				while cell.next ~= nil do
					var nextcell = cell.next
					cell.next = HashCell.alloc():copy(nextcell)
					cell = cell.next
				end
			end
		end
		self.__size = other.__size
	end

	terra HashMap:clear()
		for i=0,self.__capacity do
			if self.__cells[i] ~= nil then
				self.__cells[i]:delete()
				self.__cells[i] = nil
			end
		end
		self.__size = 0
	end

	terra HashMap:__destruct()
		self:clear()
		S.free(self.__cells)
	end

	terra HashMap:capacity() return self.__capacity end
	HashMap.methods.capacity:setinlined(true)

	terra HashMap:size() return self.__size end
	HashMap.methods.size:setinlined(true)

	terra HashMap:hash(key: K)
		return hashfn(key) % self.__capacity
	end
	HashMap.methods.hash:setinlined(true)

	terra HashMap:getPointer(key: K)
		var cell = self.__cells[self:hash(key)]
		while cell ~= nil do
			if util.equal(cell.key, key) then
				return &cell.val
			end
			cell = cell.next
		end
		return nil
	end

	-- Searches for a value matching 'key', and
	--    will create an entry if it doesn't find one.
	--    (Similar to the std::hash_map's [] operator)
	-- NOTE: This will attempt initialize the new entry's
	--    value field by calling a no-argument constructor,
	--    which will be a compile-time error if no such
	--    constructor exists.
	terra HashMap:getOrCreatePointer(key: K)
		var index = self:hash(key)
		var cell = self.__cells[index]
		while cell ~= nil do
			if util.equal(cell.key, key) then
				return &cell.val, true
			end
			cell = cell.next
		end
		-- Didn't find it; need to create
		-- (insert at head of list)
		cell = HashCell.alloc():init(key)
		cell.next = self.__cells[index]
		self.__cells[index] = cell
		self.__size = self.__size + 1
		self:__checkExpand()
		return &cell.val, false
	end

	terra HashMap:get(key: K, outval: &V)
		var vptr = self:getPointer(key)
		if vptr == nil then
			return false
		else
			@outval = @vptr
			return true
		end
	end

	-- Dies if the key does not exist
	-- Does not copy the return value
	HashMap.metamethods.__apply = terra(self: &HashMap, key: K)
		var vptr = self:getPointer(key)
		S.assert(vptr ~= nil)
		return @vptr
	end

	-- Expand and rehash
	terra HashMap:__expand()
		var oldcap = self.__capacity
		var oldcells = self.__cells
		var old__size = self.__size
		self:__init(2*oldcap)
		self.__size = old__size
		for i=0,oldcap do
			var cell = oldcells[i]
			while cell ~= nil do
				var index = self:hash(cell.key)
				var nextCellToProcess = cell.next
				cell.next = self.__cells[index]
				self.__cells[index] = cell
				cell = nextCellToProcess
			end
		end
		S.free(oldcells)
	end

	terra HashMap:__checkExpand()
		if [float](self.__size)/self.__capacity > loadFactor then
			self:__expand()
		end
	end
	HashMap.methods.__checkExpand:setinlined(true)

	terra HashMap:put(key: K, val: V) : {}
		var index = self:hash(key)
		var cell = self.__cells[index]
		if cell == nil then
			cell = HashCell.alloc():init(key, val)
			self.__cells[index] = cell
		else
			-- Check if this key is already present, and if so, replace
			-- its value
			var origcell = cell
			while cell ~= nil do
				if util.equal(cell.key, key) then
					S.rundestructor(cell.val)
					cell.val = val
					return
				end
				cell = cell.next
			end
			cell = origcell
			-- Otherwise, insert new cell at head of linked list
			var newcell = HashCell.alloc():init(key, val)
			newcell.next = cell
			self.__cells[index] = newcell
		end
		self.__size = self.__size + 1
		self:__checkExpand()
	end

	terra HashMap:remove(key: K)
		var index = self:hash(key)
		var cell = self.__cells[index]
		var prevcell : &HashCell = nil
		while cell ~= nil do
			if util.equal(cell.key, key) then
				-- CASE: Found it in the first cell
				if prevcell == nil then
					self.__cells[index] = cell.next
					return
				-- CASE: Found it in a cell further along
				else
					prevcell.next = cell.next
				end
				self.__size = self.__size - 1
				cell:delete()
				return
			end
			prevcell = cell
			cell = cell.next
		end
	end

	HashMap.metamethods.__for = function(syms, iter, body)
		local k = symbol()
		local v = symbol()
		-- v is an assignable lvalue but k is not
		return {`k, `@v}, quote
			var self = iter
			for currblock=0,self.__capacity do
				var currcell = self.__cells[currblock]
				while currcell ~= nil do
					var [k] = currcell.key
					var [v] = &(currcell.val)
					body
					currcell = currcell.next
				end
			end
		end
	end


	return HashMap
	
end)

return HM




