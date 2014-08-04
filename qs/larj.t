local util = terralib.require("qs.lib.util")

local S = terralib.require("qs.lib.std")
local tmath = terralib.require("qs.lib.tmath")
local qs = terralib.require("qs.globals")
local trace = terralib.require("qs.trace")
local random = terralib.require("qs.lib.random")
local mcmc = terralib.require("qs.mcmc")

local C = terralib.includecstring [[
#include <stdio.h>
]]


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
	terra RandomChoicePair:isAnnealingOut() return self.exists1 and not self.exists2 end
	terra RandomChoicePair:isAnnealingIn() return self.exists2 and not self.exists1 end
	RandomChoicePair.methods.isAnnealingOut:setinlined(true)
	RandomChoicePair.methods.isAnnealingIn:setinlined(true)

	-- To do proposals, we have one of our random choices propose, and then we set the value
	--    of the other one.
	terra RandomChoicePair:proposal() : {real, real}
		var fwdlp, rvslp = self.choice1:proposal()
		if self.choice2 ~= nil then
			self.choice2:setStoredValue(self.choice1:getStoredValue())
		end
	end

	-- Forward other method calls.
	-- If the method name starts with "get", then forward only to choice1.
	-- If the method name starts with "set", then forward to both choices.
	local getForwardingMethod = S.memoize(function(methodname, ...)
		local argtypes = terralib.newlist({...})
		local argsyms = argtypes:map(function(t) return symbol(t) end)
		local function startswith(str, start)
		   return string.sub(str, 1, string.len(start)) == start
		end
		return terra(self: &RandomChoicePair, [argsyms])
			escape
				if startswith(methodname, "get") then
					emit quote
						return self.choice1:[methodname]([argsyms])
					end
				elseif startswith(methodname, "set") then
					emit quote
						var ret = self.choice1:[methodname]([argsyms])
						if self.choice2 ~= nil then
							self.choice2:[methodname]([argsyms])
						end
						return ret
					end
				else
					error(string.format("larj.RandomChoicePair - Attempt to forward unrecognized method '%s'", methodname))
				end
			end
		end
	end)
	RandomChoicePair.metamethods.__methodmissing = macro(function(methodname, self, ...)
		local args = terralib.newlist({...})
		local argtypes = args:map(function(a) return a:gettype() end)
		local forwardmethod = getForwardingMethod(methodname, unpack(argtypes))
		local Tself = self:gettype()
		return `forwardmethod([Tself:ispointertostruct() and self or (`&self)], [args])
	end)

	return RandomChoicePair

end)



local function isInterpolationTrace(TraceType)
	return rawget(TraceType, "__isLarjInterpTrace")
end



-- A trace that interpolates the log probabilities of two other traces
local InterpTrace
local InterpolationTrace = S.memoize(function(RandExecTrace)

	local struct InterpolationTrace(trace.Object)
	{
		trace1: RandExecTrace,
		trace2: RandExecTrace,
		alpha: qs.float
	}

	InterpolationTrace.__isLarjInterpTrace = true

	-- How to convert this type into the equivalent type with a different 'real' type
	function InterpolationTrace.withRealType(real) return InterpTrace(RandExecTrace.withRealType(real)) end

	-- For every random choice type used by the RandExecTrace type, create a list of paired choices
	local rcTypeIndices = {}
	local function listForIndex(i) return string.format("rlist%d", i) end
	local function listForRCType(self, RCType) return `self.[listForIndex(rcTypeIndices[RCType])] end
	for i,RCType in ipairs(RandExecTrace.RandomChoiceTypes) do
		if not RCType.isStructural then
			rcTypeIndices[RCType] = i
			local ListType = S.Vector(RandomChoicePair(RCType))
			InterpolationTrace.entries:insert({field=listForIndex(i), type=ListType})
		end
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

	-- Some code requires a no-arg initializer for traces
	terra InterpolationTrace:__init()
		self:initmembers()
	end

	terra InterpolationTrace:__init(t1: &RandExecTrace, t2: &RandExecTrace)
		S.copy(self.trace1, @t1)
		S.copy(self.trace2, @t2)
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
	function InterpolationTrace.__copyFromRealType(real)
		return terra(self: &InterpolationTrace, other: &InterpolationTrace.withRealType(real))
			[trace.copyFromRealType(real)](self.trace1, other.trace1)
			[trace.copyFromRealType(real)](self.trace2, other.trace2)
			self.alpha = other.alpha
			self:buildPairedChoiceLists()
		end
	end

	terra InterpolationTrace:buildPairedChoiceLists()
		[forAllNonStructRCTypes(function(rct)
			return quote
				[listForRCType(self, rct)]:init()
				-- Look through all addresses in trace1 to get all choices shared by both
				--    traces and all choices unique to trace1, plus some (but not necessarily all)
				--    choices unique to trace2.
				for addr,clist1 in [RandExecTrace.rdbForType(`self.trace1, rct)].choicemap do
					var clist2 = [RandExecTrace.rdbForType(`self.trace2, rct)].choicemap:getPointer(addr)
					var n1 = clist1.choices:size()
					var n2 = 0
					if clist2 ~= nil then n2 = clist2.choices:size() end
					var minN = n1; if n2 < n1 then minN = n2 end
					-- Choices shared by both traces
					for i=0,minN do
						var pair = [listForRCType(self, rct)]:insert()
						pair:init(&clist1.choices(i).choice, &clist2.choices(i).choice)
					end
					-- Choices that only trace1 has
					for i=minN,n1 do
						var pair = [listForRCType(self, rct)]:insert()
						pair:init(&clist1.choices(i).choice, 1)
					end
					-- Choices that only trace2 has
					for i=n1,n2 do
						var pair = [listForRCType(self, rct)]:insert()
						pair:init(&clist2.choices(i).choice, 2)
					end
				end
				-- Now look through all addresses in trace2 to get any choices unique to trace2
				for addr,clist2 in [RandExecTrace.rdbForType(`self.trace2, rct)].choicemap do
					var clist1 = [RandExecTrace.rdbForType(`self.trace1, rct)].choicemap:getPointer(addr)
					if clist1 == nil then
						for i=0,clist2.choices:size() do
							var pair = [listForRCType(self, rct)]:insert()
							pair:init(&clist2.choices(i).choice, 2)
						end
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
			-- error(string.format("no field %s in terra object of type %s",
			-- 					entryname, tostring(InterpolationTrace)))
			return `self.trace1.[entryname]
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
						local i = symbol(uint64)
						local c = symbol(uint64)
						return quote
							var [i] = self.index - base
							var [c] = [listForRCType(`self.owner, rct)]:size()
							if [i] < [c] then
								var rc = [listForRCType(`self.owner, rct)]:get([i])
								return rc:[methodname]([argsyms])
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
InterpTrace = InterpolationTrace


-- Performs Locally-annealed reversible-jump MCMC by proposing a random change to a random
--    structural variable and then running an inner "annealing kernel" to smooth out
--    disruptions before making an accept/reject decision.
-- IMPORTANT: The annealing kernel should only apply to non-structural choices.
-- params are:
--    * annealKernel: defaults to qs.TraceMHKernel({doStruct=false})
--    * intervals: How many interpolation increments to use (defaults to 0, i.e. vanilla
--         reversible-jump MCMC)
--    * stepsPerInt: How many iterations to run the annealing kernel for per increment
--         (defaults to 1)
local function LARJKernel(params)
	params = params or {}
	local annealKernel = params.annealKernel or mcmc.exports.TraceMHKernel({doStruct=false})
	local params = params or {}
	local intervals = params.intervals or 0
	local stepsPerInt = params.stepsPerInt or 1

	return function(TraceType)

		local AnnealKernel = annealKernel(InterpolationTrace(TraceType))
		
		local struct LARJKernel(S.Object)
		{
			annealingKernel: AnnealKernel,
			intervals: uint64,
			stepsPerInt: uint64
		}
		mcmc.KernelPropStats(LARJKernel)

		terra LARJKernel:__doinit(intervals: uint64, stepsPerInt: uint64)
			self.annealingKernel:init()
			self.intervals = intervals
			self.stepsPerInt = stepsPerInt

			self:initKernelPropStats()
		end

		LARJKernel.methods.__init = macro(function(self)
			return `self:__doinit(intervals, stepsPerInt)
		end)

		terra LARJKernel:printStats(outstream: &C.FILE)
			escape
				if AnnealKernel:getmethod("proposalsMade") and AnnealKernel:getmethod("proposalsAccepted") then
					emit quote
						var pm = self.annealingKernel:proposalsMade()
						var pa = self.annealingKernel:proposalsAccepted()
						C.fprintf(outstream, "   Annealing Acceptance Ratio: %u/%u (%g%%)\n",
							pa, pm, 100.0*double(pa)/pm)
					end
				end
			end
		end

		terra LARJKernel:next(currTrace: &TraceType, iter: uint, numiters: uint)
			self.propsMade = self.propsMade + 1

			-- Make two copies of the current trace: one will keep its current structure,
			--    the other will take on some new structure after a proposal.
			var oldStructTrace = TraceType.salloc():copy(currTrace)
			var newStructTrace = TraceType.salloc():copy(currTrace)

			-- Pick a structural variable uniformly at random and propose a change to it
			var oldnumchoices = [TraceType.countChoices({isStructural=true})](newStructTrace)
			var randindex = random.random() * oldnumchoices
			var rc = [TraceType.getChoice({isStructural=true})](newStructTrace, randindex)
			var fwdPropLP, rvsPropLP = rc:proposal()
			newStructTrace:update(true)

			-- Account for the forward part of the dimension-jumping probability.
			fwdPropLP = fwdPropLP + newStructTrace.newlogprob
			fwdPropLP = fwdPropLP - tmath.log(double(oldnumchoices))

			-- Do annealing, if more than zero steps were specified
			var annealLpRatio = 0.0
			if self.intervals > 0 and self.stepsPerInt > 0 then
				var totaliters = self.intervals*self.stepsPerInt
				var lerpTrace = [InterpolationTrace(TraceType)].salloc():init(oldStructTrace, newStructTrace)
				for ival=0,self.intervals do
					lerpTrace.alpha = ival/(self.intervals-1.0)
					for step=0,self.stepsPerInt do
						var curriter = ival*self.stepsPerInt + step
						annealLpRatio = annealLpRatio + lerpTrace.logprob
						self.annealingKernel:next(lerpTrace, curriter, totaliters)
						annealLpRatio = annealLpRatio - lerpTrace.logprob
					end
				end
				oldStructTrace:destruct()
				newStructTrace:destruct()
				S.copy(@oldStructTrace, lerpTrace.trace1)
				S.copy(@newStructTrace, lerpTrace.trace2)
			end

			-- Accout for the reverse part of the dimension-jumping probability.
			rvsPropLP = rvsPropLP + oldStructTrace:lpDiff(newStructTrace)
			var newnumchoices = [TraceType.countChoices({isStructural=true})](newStructTrace)
			rvsPropLP = rvsPropLP - tmath.log(double(newnumchoices))

			-- Do accept/reject
			var acceptProb = (newStructTrace.logprob - currTrace.logprob) + rvsPropLP - fwdPropLP + annealLpRatio
			if newStructTrace.conditionsSatisfied and tmath.log(random.random()) < acceptProb then
				-- Accept
				self.propsAccepted = self.propsAccepted + 1
				util.swap(@currTrace, @newStructTrace)
			end
		end

		return LARJKernel
	end
end


return
{
	InterpolationTrace = InterpolationTrace,
	isInterpolationTrace = isInterpolationTrace,
	exports = 
	{
		LARJKernel = LARJKernel
	}
}







