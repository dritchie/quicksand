-- Re-implementation of stan's "stack_alloc.hpp" in pure Terra to allow for inlining.
-- See STAN_ROOT/src/stan/memory/stack_alloc.hpp for more documentation.

local S = require("qs.lib.std")


local terra is_aligned(ptr: &int8, bytes_aligned: uint)
	return [uint64](ptr) % bytes_aligned == 0U
end
is_aligned:setinlined(true)

local DEFAULT_INITIAL_BYTES = 65536		-- 64 KB

local terra eight_byte_aligned_malloc(size: uint) : &int8
	var ptr = [&int8](S.malloc(size))
	if ptr == nil then return ptr end 	-- malloc failed to alloc
	if not is_aligned(ptr, 8U) then
		S.printf("memoryPool.t: invalid alignment to 8 bytes, ptr=%p\n", ptr)
		S.exit(1)
	end
	return ptr
end
eight_byte_aligned_malloc:setinlined(true)

local struct MemoryPool(S.Object)
{
	blocks_ : S.Vector(&int8),
	sizes_ : S.Vector(uint),
	cur_block_ : uint,
	cur_block_end_ : &int8,
	next_loc_ : &int8,

	-- Analytics
	currAlloced: uint,
	maxAlloced: uint,
}

terra MemoryPool:__init() : {}
	var initial_nbytes = DEFAULT_INITIAL_BYTES
	self.blocks_:init(); self.blocks_:insert(eight_byte_aligned_malloc(initial_nbytes))
	self.sizes_:init(); self.sizes_:insert(initial_nbytes)
	self.cur_block_ = 0
	self.cur_block_end_ = self.blocks_(0) + initial_nbytes
	self.next_loc_ = self.blocks_(0)

	if self.blocks_(0) == nil then
		S.printf("memoryPool.t: bad alloc")
		S.exit(1)
	end

	self.currAlloced = 0U
	self.maxAlloced = 0U
end

terra MemoryPool:__destruct()
	-- Free all blocks
	for i=0,self.blocks_:size() do
		if self.blocks_(i) then
			S.free(self.blocks_(i))
		end
	end
end

terra MemoryPool:__move_to_next_block(len: uint)
	var result : &int8
	self.cur_block_ = self.cur_block_ + 1
	-- Find the next block (if any) containing at least len bytes
	while (self.cur_block_ < self.blocks_:size()) and
		  (self.sizes_(self.cur_block_) < len) do
		  self.cur_block_ = self.cur_block_ + 1
	end
	-- Allocate a new block if necessary
	if self.cur_block_ >= self.blocks_:size() then
		var newsize = self.sizes_(self.sizes_:size()-1)*2
		if newsize < len then
			newsize = len
		end
		self.blocks_:insert(eight_byte_aligned_malloc(newsize))
		if self.blocks_(self.blocks_:size()-1) == nil then
			S.printf("memoryPool.t: bad alloc")
			S.exit(1)
		end
		self.sizes_:insert(newsize)
	end
	result = self.blocks_(self.cur_block_)
	-- Get the object's state back in order.
	self.next_loc_ = result + len
	self.cur_block_end_ = result + self.sizes_(self.cur_block_)
	return result
end

terra MemoryPool:alloc(len: uint)
	-- Typically, just return and increment the next location.
	var result = self.next_loc_
	self.next_loc_ = self.next_loc_ + len
	-- Occasionally, we have to switch blocks.
	if self.next_loc_ >= self.cur_block_end_ then
		result = self:__move_to_next_block(len)
	end
	self.currAlloced = self.currAlloced + len
	return result
end
-- MemoryPool.methods.alloc:setinlined(true)

terra MemoryPool:recoverAll()
	self.cur_block_ = 0
	self.next_loc_ = self.blocks_(0)
	self.cur_block_end_ = self.next_loc_ + self.sizes_(0)

	if self.currAlloced > self.maxAlloced then
		self.maxAlloced = self.currAlloced
	end
	self.currAlloced = 0
end

terra MemoryPool:freeAll()
	-- Free all but the first block
	for i=1,self.blocks_:size() do
		if self.blocks_(i) then
			S.free(self.blocks_(i))
		end
	end
	-- Abstraction violation...
	self.sizes_._size = 1
	self.blocks_._size = 1
	self:recoverAll()
end

terra MemoryPool:currAmountAllocated()
	return self.currAlloced
end
MemoryPool.methods.currAmountAllocated:setinlined(true)

terra MemoryPool:maxAmountAllocated()
	return self.maxAlloced
end
MemoryPool.methods.maxAmountAllocated:setinlined(true)


return MemoryPool







