local util = terralib.require("qs.lib.util")

local S = util.require("lib.std")
local tmath = util.require("lib.tmath")
local qs = util.require("globals")
local trace = util.require("trace")



-- A random choice that associates two address-identical random choices from different traces.
-- These provide a layer of abstraction that helps kernel code treat InterpolationTraces (see below)
--    as if they were just a single trace.
local RandomChoicePair = S.memoize(function(RCType)

	local real = RCType.RealType

	local struct RandomChoicePair(S.Object)
	{
		choice1: &RCType,
		choice2: &RCType,
		exists1: bool,
		exists2: bool
	}

	-- ctor 1: both random choices exist
	terra RandomChoicePair:__init(c1: &RCType, c2: &RCType)
		self.choice1 = c1
		self.choice2 = c2
		self.exists1 = true
		self.exists2 = true
	end

	-- ctor 2: this choice only exists in one of the two traces
	-- which is '1' or '2'
	terra RandomChoicePair:__init(c1: &RCType, which: uint8)
		self.choice1 = c1
		self.choice2 = nil
		self.exists1 = (which == 1)
		self.exists2 = (which == 2)
	end

	-- Query whether this choice exists in each trace
	terra RandomChoicePair:existsInTrace1() return self.exists1 end
	terra RandomChoicePair:existsInTrace2() return self.exists2 end
	RandomChoicePair.method.existsInTrace1:setinlined(true)
	RandomChoicePair.method.existsInTrace2:setinlined(true)

	-- To do proposals, we have one of our random choices propose, and then we set the value
	--    of the other one.
	terra RandomChoicePair:proposal() : {real, real}
		var fwdlp, rvslp = self.choice1:proposal()
		if self.choice2 ~= nil then
			self.choice2:setUntransformedValue(self.choice1:getUntransformedValue())
		end
	end

	-- Forward other method calls
	local getForwardingMethod = S.memoize(function(methodname, ...)
		local argtypes = terralib.newlist({...})
		local argsyms = argtypes:map(function(t) return symbol(t) end)
		-- We forward the method to choice1. If the return value has unit type, then
		--    we also forward it to choice2. The idea is that for methods that return
		--    something, we only need to fetch that something from one of the two
		--    choices.
		local maybeForwardToChoice2 = macro(function(self, choice1RetVal)
			if choice1RetVal:gettype() == tuple() then
				return quote
					if self.choice2 ~= nil then
						self.choice2:[methodname]([argsyms])
					end
				end
			else return quote end end
		end)
		return terra(self: &RandomChoicePair, [argsyms])
			var ret = self.choice1:[methodname]([argsyms])
			maybeForwardToChoice2(self, ret)
			return ret
		end
	end)
	RandomChoicePair.metamethods.__methodmissing = macro(function(methodname, self, ...)
		local args = terralib.newlist({...})
		local argtypes = args:map(function(a) return a:gettype() end)
		return `[getForwardingMethod(methodname, unpack(argtypes))](self, [args])
	end)

	return RandomChoicePair

end)




-- A trace that interpolates the log probabilities of two other traces
local InterpolationTrace = S.memoize(function(RandExecTrace)

	local struct InterpolationTrace(S.Object)
	{
		trace1: RandExecTrace,
		trace2: RandExecTrace,
		alpha: qs.primfloat
	}

	-- For every random choice type used by the RandExecTrace type, create a list of paired choices
	local rcTypeIndices = {}
	local function listForRCType(self, RCType) return string.format("rlist%d", rcTypeIndices[RCType]) end
	for i,RCType in ipairs(RandExecTrace.RandomChoiceTypes) do
		rcTypeIndices[RCType] = i
		local ListType = S.Vector(RandomChoicePair(RCType))
		InterpolationTrace.entries:insert({field=listForRCType(RCType), type=ListType})
	end
	local function forAllNonStructRCTypes(fn)
		return quote
			escape	
				for i,RCType in ipairs(RandExecTrace.RandomChoiceTypes) do
					if not RCType.isStructural then
						emit quote [fn(RCType)] end
					end
				end
			end
		end
	end

	-- NOTE: Assumes ownership of t1 and t2 (i.e. does not copy)
	terra InterpolationTrace:__init(t1: &RandExecTrace, t2: &RandExecTrace)
		self.trace1 = @t1
		self.trace2 = @t2
		self.alpha = 0.0
		self:buildPairedChoiceLists()
	end

	-- Could almost just use the default copy ctor, but just copying over
	--    the paired choice lists will result in stale pointers, so we need to 
	--    rebuild that.
	terra InterpolationTrace:__copy(other: &InterpolationTrace)
		S.copy(self.trace1, other.trace1)
		S.copy(self.trace2, other.trace2)
		self.alpha = other.alpha
		self:buildPairedChoiceLists()
	end

	terra InterpolationTrace:buildPairedChoiceLists()
		[forAllNonStructRCTypes(function(rct)
			[listForRCType(self, rct)]:init()
			-- Look through all addresses in trace1 to get all choices shared by both
			--    traces and all choices unique to trace1, plus some (but not necessarily all)
			--    choices unique to trace2.
			for addr,clist1 in [RandExecTrace.rdbForType(`self.trace1, rct)].choicemap do
				var clist2 = self.trace2:getPointer(addr)
				var n1 = clist1:size()
				var n2 = 0
				if clist2 ~= nil then n2 = clist2:size() end
				var minN = n1; if n2 < n1 then minN = n2 end
				-- Choices shared by both traces
				for i=0,minN do
					var pair = [listForRCType(self, rct)]:insert()
					pair:init(&clist1(i).choice, &clist2(i).choice)
				end
				-- Choices that only trace1 has
				for i=minN,n1 do
					var pair = [listForRCType(self, rct)]:insert()
					pair:init(&clist1(i).choice, 1)
				end
				-- Choices that only trace2 has
				for i=n1,n2 do
					var pair = [listForRCType(self, rct)]:insert()
					pair:init(&clist2(i).choice, 2)
				end
			end
			-- Now look through all addresses in trace2 to get any choices unique to trace2
			for addr,clist2 in [RandExecTrace.rdbForType(`self.trace2, rct)].choicemap do
				var clist1 = self.trace1:getPointer(addr)
				if clist1 == nil then
					for i=0,clist2:size() do
						var pair = [listForRCType(self, rct)]:insert()
						pair:init(&clist2(i).choice, 2)
					end
				end
			end
		end)]
	end

	InterpolationTrace.methods.lerp = macro(function(self, one, two)
		return `(1.0 - self.alpha)*one + self.alpha*two
	end)

	-- Auto-generate accessors for logprob, conditionsSatisfied, etc. that
	--    do the natural, correct thing.
	InterpolationTrace.metamethods.__entrymissing = macro(function(entryname, self)
		local isLogprobEntry = entryname == "logprob" or
							   entryname == "loglikelihood" or
							   entryname == "newlogprob" or
							   entryname == "oldlogprob"
		if isLogprobEntry then
			return `self:lerp(self.trace1.[entryname], self.trace2.[entryname])
		elseif entryname == "conditionsSatisfied" then
			return `self.trace1.conditionsSatisfied and self.trace2.conditionsSatisfied
		else
			error(string.format("no field %s in terra object of type %s",
								entryname, tostring(InterpolationTrace)))
		end
	end)

	-- Forward method calls to sub traces, too (e.g. update, setTemperature)
	InterpolationTrace.metamethods.__methodmissing = macro(function(methodname, self, ...)
		local args = {...}
		return quote
			-- There are no trace methods with return values, so don't bother
			self.trace1:[methodname]([args])
			self.trace2:[methodname]([args])
		end
	end)


	-- OK, now the really annoying part of having to reproduce random choice forwarding

	InterpolationTrace.countChoices = trace.memoizeOnPredicate(function(predfn)
		return terra(self: &InterpolationTrace)
			var count: uint64 = 0
			[forAllNonStructRCTypes(function(rct)
				if predfn(rct) then
					return quote count = count + [listForRCType(self, rct)]:size() end
				else return quote end end
			end)]
			return count
		end
	end)

	InterpolationTrace.getChoice = trace.memoizeOnPredicate(function(predfn)

		local struct Proxy
		{
			owner: &InterpolationTrace,
			index: uint64
		}

		local getForwardingMethod = S.memoize(function(methodname, ...)
			local argtypes = terralib.newlist({...})
			local argsyms = argtypes:map(function(t) return symbol(t) end)
			local terra forwardmethod(self: &Proxy, [argsyms])
				var base : uint64 = 0
				[forAllNonStructRCTypes(function(rct)
					if predfn(rct) then
						local method = rct:getmethod(methodname)
						local i = symbol(uint64)
						local c = symbol(uint64)
						return quote
							var [i] = self.index - base
							var [c] = [listForRCType(`self.owner, rct)]:size()
							if [i] < [c] then
								var rc = [listForRCType(`self.owner, rct)]:get([i])
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
					"Return type of random choice method '%s' not uniquely determined by the provided InterpolationTrace.getChoice predicate",
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

		return terra(self: &InterpolationTrace, index: uint64)
			return Proxy { self, index }
		end

	end)


	return InterpolationTrace

end)






