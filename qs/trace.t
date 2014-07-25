local util = terralib.require("qs.lib.util")

local S = util.require("lib.std")
local globals = util.require("globals")
local Hash = util.require("lib.hash")
local HashMap = util.require("lib.hashmap")



------------------------------------
---  Trace compilation machinery ---
------------------------------------

-- The program currently being compiled
local currentProgram = nil

-- The value of globals.real before compilation was invoked.
-- We restore this when compilation is finished.
local prevReal = nil

-- Compiling probabilistic programs proceeds in two passes.
-- The first pass detects the types of all random choices used in the program.
-- This flag indicates whether we are in the first pass.
local rcTypeDetectionPass = false

-- This is where we record the types of all random choices used.
local rcTypesUsed = {}

-- Signalling functions exposed to code in other files
local compilation = {}
function compilation.isCompiling()
	return currentProgram ~= nil
end
function compilation.currentlyCompilingProgram()
	return currentProgram
end
function compilation.beginCompilation(program, real)
	currentProgram = program
	prevReal = globals.real
	globals.real = real
end
function compilation.beginRCTypeDetectionPass()
	rcTypesUsed = {}
	rcTypeDetectionPass = true
end
function compilation.isDoingTypeDetectionPass()
	return rcTypeDetectionPass
end
function compilation.endRCTypeDetectionPass()
	rcTypeDetectionPass = false
end
function compilation.endCompilation()
	currentProgram = nil
	globals.real = prevReal
end



-------------------------------------
---  Random execution trace type  ---
-------------------------------------

local Address = S.Vector(int)

local terra hashAddress(addr: Address)
	return Hash.rawhash([&int8](addr:get(0)), sizeof(int)*addr:size())
end
hashAddress:setinlined(true)

-- Database of values for a particular type of random choice
local RandomDB = S.memoize(function(RandomChoiceT)

	-- A random choice, plus an index that identifies at what point
	--    in the program execution order this random choice was made
	-- (e.g. index 0 = the first random choice made)
	local struct ChoiceWithIndex(S.Object)
	{
		choice: RandomChoiceT,
		index: uint64
	}

	-- A list of indexed random choices, plus a counter
	-- This represents the list of random choices that have been made at a particular
	--    lexical address.
	-- The counter keeps track of how many times we've hit this address in the 
	--    current program execution.
	local struct IndexedChoicesWithCounter(S.Object)
	{
		choices: S.Vector(ChoiceWithIndex),
		counter: uint64
	}
	terra IndexedChoicesWithCounter:__init()
		self:initmembers()
		self.counter = 0
	end

	-- A list of pointers to random choices, plus a counter.
	-- We use this to store a 'flat' list of random choices, which provides
	--    faster lookup during structure-invariant program executions.
	-- The counter keeps track of which random choice we should look up next.
	local struct ChoicePointersWithCounter(S.Object)
	{
		pointers: S.Vector(&RandomChoiceT),
		counter: uint64
	}
	terra ChoicePointersWithCounter:__init()
		self:initmembers()
		self.counter = 0
	end

	-- Stores both a structured map from addresses to choices as well
	--    as a flat list of choices.
	-- Can use the second when we know the control flow structure of the
	--    program is not changing.
	local struct RandomDBT(S.Object)
	{
		addressStack: &Address,
		-- TODO: If we have lots of random choices happening deep in the call stack,
		--    a hash table will end of storing a ton of redundant information (a complete
		--    copy of the adddress stack for each key). Maybe experiment with tries instead?
        --    (expected nlogn instead of constant lookup, though...)
		choicemap: HashMap(Address, IndexedChoicesWithCounter, hashAddress),
		choicelist: ChoicePointersWithCounter
	}

	terra RandomDBT:__init(addressStack: &Address)
		self:initmembers()
		S.assert(self.choicemap.__cells ~= nil)
		self.addressStack = addressStack
	end

	-- Can almost just use the default copy constructor, but for:
	--    * Need to clear out the flat list of pointers (since those will point to the copy source)
	terra RandomDBT:__copy(other: &RandomDBT)
		self:copymembers(other)
		self.choicelist.pointers:clear()
	end

	terra RandomDBT:lookupNonStructural()
		-- Just retrieve the next choice in the flat list, and then
		--   increment the index.
		var x = self.choicelist.pointers(self.choicelist.counter)
		self.choicelist.counter = self.choicelist.counter + 1
		return x
	end

	-- Returns the random choice, plus a boolean indicating whether the choice
	--    was retrieved (true) or had to be created (false).
	local function lookupStructural(self, initArgs)
		return quote
			-- Retrieves the list of choices made at this lexical address (or creates an
			--    empty list if no choices have yet been made here)
			var clist, foundit = self.choicemap:getOrCreatePointer(@self.addressStack)
			-- If we've hit this lexical address more times than we have stored choices, then
			--    we need to create a new database entry for the new choice.
			if clist.choices:size() <= clist.counter then
				foundit = false
				-- Add a new random choice database entry
				clist.choices:insert()
				-- Initialize it
				clist.choices(clist.choices:size()-1).choice:init([initArgs])
			end
			var ichoice = clist.choices:get(clist.counter)
			-- Record the fact that we've hit this lexical position one more time
			clist.counter = clist.counter + 1
			-- Record the index in the program execution order where this choice occured
			ichoice.index = self.choicelist.counter
			self.choicelist.counter = self.choicelist.counter + 1
		in
			&ichoice.choice, foundit
		end
	end

	-- Make a method for each overload of RandomChoiceT:init
	for _,def in ipairs(RandomChoiceT.methods.__init:getdefinitions()) do
		local argTypes = def:gettype().parameters
		local argsyms = terralib.newlist()
		-- Skip argTypes[1], which is the self argument
		for i=2,#argTypes do
			argsyms:insert(symbol(argTypes[i]))
		end
		terra RandomDBT:lookupStructural([argsyms])
			return [lookupStructural(self, argsyms)]
		end
	end

	terra RandomDBT:setAddressStack(addressStack: &Address)
		self.addressStack = addressStack
	end
	RandomDBT.methods.setAddressStack:setinlined(true)


	-- Clear out everything
	terra RandomDBT:clear()
		self.choicemap:clear()
		self.choicelist.pointers:clear()
	end

	-- Rebuild the flat list of random choice pointers.
	-- This is done on-demand, as needed (e.g. for nonstructural updates, or when asking
	--    for a particular random choice, etc.)
	terra RandomDBT:ensureFlatListIsBuilt()
		if self.choicelist.pointers:size() == 0 then
			-- self.choicelist.counter tells the number of random choices
			--    made on the last execution
			self.choicelist.pointers:reserve(self.choicelist.counter)
			-- This is a total abstraction violation, but it's the fastest way to do what I want...
			self.choicelist.pointers._size = self.choicelist.counter
			for addr,clist in self.choicemap do
				for ichoice in clist.choices do
					-- ichoice.index tells where in the execution order the choice was made.
					self.choicelist.pointers(ichoice.index) = &ichoice.choice
				end
			end
		end
	end

	-- Prepare for a trace update
	terra RandomDBT:prepareForTraceUpdate(canStructureChange: bool)
		-- If non-structural:
		--    rebuild the flat list of choice pointers, if needed
		if not canStructureChange then
			self:ensureFlatListIsBuilt()
		-- If structural:
		--    clear the flat choice list
		--    reset the per-address loop counters
		--    mark all variables as inactive
		else
			self.choicelist.pointers:clear()
			for addr,clist in self.choicemap do
				clist.counter = 0
				for irc in clist.choices do irc.choice.active = false end
			end
		end
		-- Always: Reset the flat list index counter
		self.choicelist.counter = 0
	end

	-- Clear out any choices that were not reachable on the last run of the program
	-- (These are the ones left marked 'inactive') 
	-- Returns the summed logprob of all removed choices
	terra RandomDBT:clearUnreachables()
		var oldlp : RandomChoiceT.RealType = 0.0
		for addr,clist in self.choicemap do
			for i=[int64](clist.choices:size()-1),-1,-1 do
				if not clist.choices(i).choice.active then
					oldlp = oldlp + clist.choices(i).choice.logprob
					S.rundestructor(clist.choices(i))
					clist.choices:remove(i)	-- Does not destruct; instead returns the removed value
					i = i + 1
				end
			end
		end
		return oldlp
	end

	-- Get the number of random choices in this rdb
	terra RandomDBT:countChoices()
		return self.choicelist.counter
	end
	RandomDBT.methods.countChoices:setinlined(true)

	-- Retrieve a random choice by index
	terra RandomDBT:getChoice(index: uint64)
		self:ensureFlatListIsBuilt()
		return self.choicelist.pointers(index)
	end

	return RandomDBT

end)


-- We create one global pointer variable for every type of trace that gets created
-- When doing inference, the appropriate one of these points to the 'currently executing trace'
-- (This is a map from trace type to global pointer variable)
local globalTracePointer = S.memoize(function(TraceType)
	return global(&TraceType, 0)	-- 0 for nil literal
end)


-- Memoize a function on a predicate. A predicate is either a function taking one argument
--    and returning bool, or a table of (property, boolvalue) pairs. If the latter, we construct
--    a predicate function that takes an argument and checks if that argument object has all of
--    the properties with those values.
-- The function being memoized will always see a predicate function: the details of how it was
--    constructed are abstracted.
-- This is used for random choice type filtering operations by RandExecTrace below.
local function memoizeOnPredicate(fn)

	local memfn = S.memoize(function(...)
		local args = {...}
		if #args == 1 and type(args[1]) == "function" then
			-- If we were passed a predicate function, just pass it through
			return fn(args[1])
		else
			-- Otherwise we need to construct a function out of a list of property,value pairs
			assert(#args % 2 == 0, "memoizeOnPredicate: found an odd number of args in prop,val pair case")
			local props = {}
			for i=1,#args/2 do
				local key = args[2*i - 1]
				local val = args[2*i]
				props[key] = val
			end
			local function predfn(x)
				for k,v in pairs(props) do
					if x[k] ~= v then return false end
				end
				return true
			end
			return fn(predfn)
		end
	end)

	return function(predFnOrTable)
		predFnOrTable = predFnOrTable or {}
		if type(predFnOrTable) == "function" then
			return memfn(predFnOrTable)
		else
			assert(type(predFnOrTable) == "table",
				"memoizeOnPredicate: argument must be either a function or a table")
			-- Unpack table into argument list for memoization.
			-- Sort by key to ensure consistent ordering.
			local keys = terralib.newlist()
			for k,_ in pairs(predFnOrTable) do keys:insert(k) end
			table.sort(keys)
			local arglist = terralib.newlist()
			for _,k in ipairs(keys) do
				arglist:insert(k)
				arglist:insert(predFnOrTable[k])
			end
			return memfn(unpack(arglist))
		end
	end
end


-- The trace type itself
local RandExecTrace
local _RandExecTrace = S.memoize(function(program, real)

	-- Compile the program
	-- (This is an asynchronous operation that will not finish until this type constructor
	--    returns. However, it will synchronously compute the 'rcTypesUsed' list and determine
	--    the return type of program)
	local tprog, RetType = program:compile(real)

	local struct RandExecTraceT(S.Object)
	{
		logprob: real,
		newlogprob: real,
		oldlogprob: real,
		temperature: real,
		conditionsSatisfied: bool,
		returnValue: RetType,
		-- This starts out false, but is forever after set to true as soon as :update() is run
		-- IMPORTANT: Everything works out fine, provided the destructor is never invoked before
		--    the first run of :update(). I don't think this should ever happen.
		hasReturnValue: bool,

		addressStack: Address,
		canStructureChange: bool
	}

	-- Add a RandomDB member for every type of random choice used
	--   by the program.
	local rcTypeIndices = terralib.newlist()
	local i = 1
	for rct,_ in pairs(rcTypesUsed) do
		rcTypeIndices[rct] = i
		RandExecTraceT.entries:insert({ field=string.format("rdb%d", i), type=RandomDB(rct) })
		i = i + 1
	end
	assert(i > 1,
		"A probabilistic program must make at least one random choice")
	local function rdbForType(self, RCType)
		return `self.[string.format("rdb%d", rcTypeIndices[RCType])]
	end
	local function emitForAllRCTypes(fn)
		return quote
			escape	
				for rct,_ in pairs(rcTypesUsed) do
					emit quote [fn(rct)] end
				end
			end
		end
	end
	local function forAllRDBs(self, fn)
		return emitForAllRCTypes(function(rct)
			return fn(rdbForType(self, rct))
		end)
	end

	-- Create the global pointer variable for this trace type
	local gTrace = globalTracePointer(RandExecTraceT)

	terra RandExecTraceT:__init()
		self.logprob = 0.0
		self.newlogprob = 0.0
		self.oldlogprob = 0.0
		self.temperature = 1.0
		self.conditionsSatisfied = false
		self.hasReturnValue = false

		self.addressStack:init()
		self.canStructureChange = true

		[forAllRDBs(self, function(rdb)
			return quote
				rdb:init(&self.addressStack)
			end
		end)]

		while not self.conditionsSatisfied do
			-- Clear out all random dbs
			[forAllRDBs(self, function(rdb)
				return quote
					rdb:clear()
				end
			end)]
			-- Try running from scratch
			self:update(true)
		end
	end

	-- Can almost just use the default copy constructor, but for a couple things:
	--    * Need to make the 'addressStack' pointers in the rdbs correct
	terra RandExecTraceT:__copy(other: &RandExecTraceT)
		self:copymembers(other)
		[forAllRDBs(self, function(rdb)
			return quote
				rdb:setAddressStack(&self.addressStack)
			end
		end)]
	end

	terra RandExecTraceT:setTemperature(temp: real)
		self.temperature = temp
	end
	RandExecTraceT.methods.setTemperature:setinlined(true)

	local nextAddress = 0
	RandExecTraceT.methods.pushAddressStack = macro(function(self)
		local address = nextAddress
		nextAddress = nextAddress + 1
		return quote
			if self.canStructureChange then
				self.addressStack:insert(address)
			end
		end
	end)

	RandExecTraceT.methods.popAddressStack = macro(function(self)
		return quote
			if self.canStructureChange then
				self.addressStack:remove()
			end
		end
	end)

	-- Also returns a boolean indicating whether the random choice was
	--    retrieved or created.
	function RandExecTraceT.lookupRandomChoice(RCType)
		return macro(function(self, ...)
			local initArgs = terralib.newlist({...})
			local initArgsTmp = initArgs:map(function(a) return symbol(a:gettype()) end)
			return quote
				var x : &RCType
				var foundit = true
				if not self.canStructureChange then
					x = [rdbForType(self, RCType)]:lookupNonStructural()
				else
					var [initArgsTmp] = [initArgs]
					self:pushAddressStack()
					x, foundit = [rdbForType(self, RCType)]:lookupStructural([initArgsTmp])
					self:popAddressStack()
					if not foundit then
						-- Increment self.newlogprob, since we just created a new random choice
						self.newlogprob = self.newlogprob + x.logprob
					end
				end
			in
				x, foundit
			end
		end)
	end

	terra RandExecTraceT:update(canStructureChange: bool)
		self.canStructureChange = canStructureChange

		-- Assume ownership of the global trace
		var prevTrace = gTrace
		gTrace = self

		self.logprob = 0.0
		self.newlogprob = 0.0
		self.oldlogprob = 0.0
		self.conditionsSatisfied = true

		-- Prepare rdbs for trace update
		[forAllRDBs(self, function(rdb)
			return quote
				rdb:prepareForTraceUpdate(canStructureChange)
			end
		end)]

		-- Run computation
		if self.hasReturnValue then S.rundestructor(self.returnValue) end
		self.returnValue = tprog()
		self.hasReturnValue = true

		-- Adjust logprobs by temperature
		self.logprob = self.logprob / self.temperature
		self.newlogprob = self.newlogprob / self.temperature
		self.oldlogprob = self.oldlogprob / self.temperature

		-- If structural, clear out unreachable variables from rdbs
		if canStructureChange then
			[forAllRDBs(self, function(rdb)
				return quote
					self.oldlogprob = self.oldlogprob + rdb:clearUnreachables()
				end
			end)]
		end

		-- Release ownership of global trace
		gTrace = prevTrace

		-- Every run of the program must make some number of random choices
		if canStructureChange then
			var count = 0
			[forAllRDBs(self, function(rdb)
				return quote
					count = count + rdb:countChoices()
				end
			end)]
			if count == 0 then
				S.printf("Execution of probabilistic program resulted in zero random choices\n")
				S.assert(false)
			end
		end
	end


	-- These two 'filter' functions allow the caller to get the number of random choices
	--    whose type matches some predicate, or to retrieve such a random choice by index.

	RandExecTraceT.countChoices = memoizeOnPredicate(function(predfn)
		return terra(self: &RandExecTraceT)
			var count: uint64 = 0
			[emitForAllRCTypes(function(rct)
				if predfn(rct) then
					return quote count = count + [rdbForType(self, rct)]:countChoices() end
				else return quote end end
			end)]
			return count
		end
	end)

	RandExecTraceT.getChoice = memoizeOnPredicate(function(predfn)

		-- What we return from this function is a proxy object that forwards
		--    all method calls to the appropriate random choice
		local struct Proxy
		{
			owner: &RandExecTraceT,
			index: uint64
		}

		local getForwardingMethod = S.memoize(function(methodname, ...)
			local argtypes = terralib.newlist({...})
			local argsyms = argtypes:map(function(t) return symbol(t) end)
			local terra forwardmethod(self: &Proxy, [argsyms])
				var base : uint64 = 0
				[emitForAllRCTypes(function(rct)
					if predfn(rct) then
						local method = rct:getmethod(methodname)
						local i = symbol(uint64)
						local c = symbol(uint64)
						return quote
							var [i] = self.index - base
							var [c] = [rdbForType(`self.owner, rct)]:countChoices()
							if [i] < [c] then
								var rc = [rdbForType(`self.owner, rct)]:getChoice([i])
								return method(rc, [argsyms])
							end
							base = base + [c]
						end
					else return quote end end
				end)]
			end
			-- Attempt to compile the forwarding function.
			local success, errmsg = pcall(forwardmethod.compile, forwardmethod)
			if not success then
				if string.find(errmsg, "incompatible types") then error(string.format(
					"Return type of random choice method '%s' not uniquely determined by the provided RandExecTrace.getChoice predicate",
					methodname))
				else
					error(errmsg)
				end
			end
			return forwardmethod
		end)

		Proxy.metamethods.__getmethod = function(self, methodname)
			return macro(function(self, ...)
				local args = terralib.newlist({...})
				local argtypes = args:map(function(a) return a:gettype() end)
				local forwardmethod = getForwardingMethod(methodname, unpack(argtypes))
				local Tself = self:gettype()
				return `forwardmethod([Tself:ispointertostruct() and self or (`&self)], [args])
			end)
		end

		return terra(self: &RandExecTraceT, index: uint64)
			return Proxy { self, index }
		end
	end)	



	return RandExecTraceT

end)
-- IMPORTANT: Due to the recursive nature of probabilistic program compilation (i.e. the same
--    recursive dependency that requires us to do some asynchronous compile calls), we need to
--    compile programs before we enter any trace type constructors that refer to them.
-- Otherwise, the type constructor ends up being called recursively, and we'll get a trace type whose
--    'tprog' function was compiled to refer to a different instantiation of what should be the
--    same trace type.
RandExecTrace = function(program, real)
	program:compile()
	return _RandExecTrace(program, real)
end


local function GlobalTraceType()
	assert(currentProgram,
		"Cannot call 'globalTrace()' outside of trace compilation")
	return RandExecTrace(currentProgram, globals.real)
end

-- Retrieve a global variable pointing to the currently executing trace for
--    a particular program type.
local function globalTrace()
	local RandExecTraceT = GlobalTraceType()
	local gTrace = globalTracePointer(RandExecTraceT)
	assert(gTrace,
		"'globalTrace()'' could not find a global trace pointer for the current program trace type. This should be impossible...")
	return gTrace
end

-- Check whether the current execution of the program is a trace recording
--    execution
local function isRecordingTrace()
	return `[globalTrace()] ~= nil
end

local printType = macro(function(x)
	print(x:gettype())
	return quote end
end)

-- Look up random choice value in the currently-executing trace
-- Non-POD values are returned by pointer
local function lookupRandomChoiceValue(RandomChoiceT, args, initVal)
	-- If we're doing the random choice type detection compiler pass, then we
	--    record the use of this type
	if rcTypeDetectionPass then
		rcTypesUsed[RandomChoiceT] = true
	end
	local ValType = RandomChoiceT.ValueType
	local initArgs = terralib.newlist()
	initArgs:insertall(args)
	if initVal then initArgs:insert(initVal) end
	return quote
		var val : ValType
		-- Only proceed with trace lookup if we're past the type detection pass
		escape
			if not rcTypeDetectionPass then
				emit quote
					-- If we're currently in a trace update execution, look up the choice value from the
					--    currently-executing trace.
					if [isRecordingTrace()] then
						var rc, foundit = [GlobalTraceType().lookupRandomChoice(RandomChoiceT)]([globalTrace()], [initArgs])
						-- If this choice was retrieved, not created, then we should check if
						--    the prior probability etc. need to be updated
						if foundit then
							rc:update([args])
						end
						-- Regardless, we need to increment the trace's log probability
						[globalTrace()].logprob = [globalTrace()].logprob + rc.logprob

						-- Retrieve and copy the value of the random choice we found/created
						var x = rc:getValue()
						S.copy(val, x)
					else
						-- If this is not part of a trace execution, just draw a forward sample
						val = [RandomChoiceT.sampleFunction]([args])
					end
					-- defer destruct if ValueType has a destructor
					escape
						if ValType:isstruct() and ValType:getmethod("destruct") then
							emit quote defer val:destruct() end
						end
					end
				end
			end
		end
	in
		-- Return pointer-to-struct, in keeping with salloc() convention,
		--    if ValType is non-POD
		[util.isPOD(RandomChoiceT.ValueType) and (`val) or (`&val)]
	end
end


-----------------------------------------------------
---  Address tracking for user-defined functions  ---
-----------------------------------------------------

-- A probabilistic function is a Terra function that makes random choices
--    (or whose callees make random choices)
-- They can be defined as:
--    local f = qs.func(terrafn)
-- Recursive functions can be defined as:
--    local f = qs.func()
--    f:define(terrafn)
local function getFuncReturnType(terrafn)
	local RetType = nil
	for _,d in ipairs(terrafn:getdefinitions()) do
		-- Check if the definition has an explicit return type.
		-- If it does not, attempt to asynchronously compile it.
		-- If this does not return the type, then we know the definition is recursive.
		local success, ftype = d:peektype()
		if not success then ftype = d:gettype(true) end
		if not ftype then
			error("Recursive definition of a qs.func must have explicit return type")
		end
		-- All overloads of the function must have the same return type
		--    (subsequent code generation assumes this)
		if RetType == nil then
			RetType = ftype.returntype
		else
			assert(ftype.returntype == RetType,
				"All overloads of a qs.func must have the same return type")
		end
	end
	return RetType
end
local function _func(terrafn, ismethod)
	local data = { def = nil }

	local qsfunc = macro(function(...)
		local args = terralib.newlist({...})
		local argtypes = args:map(function(x) return x:gettype() end)
		-- If we're wrapping a method, ensure that the first argument (i.e. self)
		--    is pointer-to-struct
		if ismethod and (not argtypes[1]:ispointertostruct()) then
			args[1] = `&args[1]
			argtypes[1] = `&argtypes[1]
		end
		local argstmp = argtypes:map(function(t) return symbol(t) end)
		local RetType = getFuncReturnType(data.def)
		return quote
			var result : RetType
			escape
				if rcTypeDetectionPass then
					emit quote result = [data.def]([args]) end
				else
					emit quote
						-- If we're not recording a trace then skip address tracking
						if not [isRecordingTrace()] then
							result = [data.def]([args])
						else
							var [argstmp] = [args]
							[globalTrace()]:pushAddressStack()
							result = [data.def]([argstmp])
							[globalTrace()]:popAddressStack()
						end
					end
				end
			end
		in
			result
		end
	end)

	qsfunc.data = data

	qsfunc.define = function(self, terrafn)
		if terrafn ~= nil then
			assert(terralib.isfunction(terrafn),
				"Definition of a qs.func must be a Terra function")
			self.data.def = terrafn
		end
	end

	qsfunc:define(terrafn)

	return qsfunc
end
local function func(terrafn)
	return _func(terrafn, false)
end
local function method(terramethod)
	return _func(terramethod, true)
end



-------------------------------------
---       Public interface        ---
-------------------------------------


-- 'factor' and 'factorfunc' allow direct increment of the currently-executing trace's log probability
-- 'factor' takes the logprob increment amount directly
-- 'factorfunc' wraps a Terra function whose return value should be used as the increment amount
-- TODO: Provide 'isEvaluatingFactors' which can be toggled off to turn off evaluation of
--    (expensive) factors when we're only running the trace to e.g. reconstruct a return value.
local factor = macro(function(num)
	if not rcTypeDetectionPass then
		return quote
			if [isRecordingTrace()] then
				[globalTrace()].logprob = [globalTrace()].logprob + num
			end
		end
	else return quote end end
end)
local function factorfunc(fn)
	assert(terralib.isfunction(fn),
		"Argument to qs.factorfunc must be a Terra function")
	for _,def in ipairs(fn:getdefinitions()) do
		assert(def:gettype().returntype == globals.real,
			"Definition of qs.factorfunc must return type 'real'")
	end
	fn = func(fn)
	return macro(function(...)
		local args = terralib.newlist({...})
		if not rcTypeDetectionPass then
			return quote
				if [isRecordingTrace()] then
					[globalTrace()].logprob = [globalTrace()].logprob + fn([args])
				end
			end
		else return quote end end
	end)
end

-- 'condition' imposes a hard constraint on the program execution space.
-- 'conditionfunc' is like 'factorfunc', but for hard constraints.
--  TODO: Use 'isEvaluatingFactors' here, too?
local condition = macro(function(pred)
	if not rcTypeDetectionPass then
		return quote
			if [isRecordingTrace()] then
				[globalTrace()].conditionsSatisfied =
					[globalTrace()].conditionsSatisfied and pred
			end
		end
	else return quote end end
end)
local function conditionfunc(fn)
	assert(terralib.isfunction(fn),
		"Argument to qs.conditionfunc must be a Terra function")
	for _,def in ipairs(fn:getdefinitions()) do
		assert(def:gettype().returntype == bool,
			"Definition of qs.conditionfunc must return type 'bool'")
	end
	fn = func(fn)
	return macro(function(...)
		local args = terralib.newlist({...})
		if not rcTypeDetectionPass then
			return quote
				if [isRecordingTrace()] then
					[globalTrace()].conditionsSatisfied =
						[globalTrace()].conditionsSatisfied and fn([args])
				end
			end
		else return quote end end
	end)
end

-- 'range' takes a min and max value (integers) and constructs a range
--     object that can be iterated with a standard for loop.
--  It also pushes and pops the address stack, so using range-based loops
--     may provide better statistical efficiency when querying programs
--     with nested loops and structure change.
local struct __Range
{
	min: int64,
	max: int64
}
__Range.metamethods.__for = function(syms,iter,body)
	return syms, quote
		var it = iter
		escape
			if not rcTypeDetectionPass then
				emit quote
					if [isRecordingTrace()] then [globalTrace()]:pushAddressStack() end
				end
			end
		end
		for [syms[1]] = it.min,it.max do
			body
		end
		escape
			if not rcTypeDetectionPass then
				emit quote
					if [isRecordingTrace()] then [globalTrace()]:popAddressStack() end
				end
			end
		end
	end
end
local terra range(min: int64, max: int64)
	return __Range { min, max }
end
range:setinlined(true)





return 
{
	compilation = compilation,
	RandExecTrace = RandExecTrace,
	lookupRandomChoiceValue = lookupRandomChoiceValue,
	factor = factor,
	exports = 
	{
		func = func,
		method = method,
		factor = factor,
		factorfunc = factorfunc,
		condition = condition,
		conditionfunc = conditionfunc,
		range = range
	}
}





