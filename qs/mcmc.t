local util = terralib.require("qs.lib.util")

local S = util.require("lib.std")
local qs = util.require("globals")
local progmod = util.require("progmodule")
local trace = util.require("trace")
local random = util.require("lib.random")
local distrib = util.require("distrib")
local tmath = util.require("lib.tmath")
local infer = util.require("infer")

local C = terralib.includecstring [[
#include <stdio.h>
inline FILE* getstderr() { return stderr; }
inline FILE* getstdout() { return stdout; }
inline void flushstderr() { fflush(stderr); }
]]



-- 'MCMC' is a function that takes a transition kernel and a set of parameters
--    and returns a sampling function for use as a query method.
-- Arguments:
--    * kernel: A function taking a Trace type and returning a Terra struct type
--              that initializes with no arguments and has a :next(&Trace) method
--              that modifies the trace in place.
--    * params: Additional parameters that can be either Lua constants or Terra quotes
--       + numsamps: How many samples to draw (default = 1000)
--		 + burnin: How many samples to skip at the beginning (default = 0)
--       + lag: How many additional samples to insert between retained samples (default = 0)
--       (The function thus runs for burnin + numsamps*lag total iterations
--       + verbose: Whether to print out stats. Can be constant true/false, or a file handle.
--                  to which output should be written (true = stdout).
--                  Will look for methods 'proposalsMade' and 'proposalsAccepted' in kernel type.
--					Will also look for method 'printStates' in kernel type.
local function MCMC(kernel, params)
	params = params or {}
	local _numsamps = params.numsamps or 1000
	local _burnin = params.burnin or 0
	local _lag = params.lag or 1
	local _verbose = params.verbose
	if not _verbose then _verbose = 0 end 	-- 0 for nil literal if param was nil or false
	if _verbose == true then _verbose = C.getstdout() end

	return function(program)
		progmod.assertIsProgram(program, "MCMC")
		local TraceType = trace.RandExecTrace(program, qs.primfloat)
		local KernelType = kernel(TraceType)

		local terra doMCMC(samples: &S.Vector(infer.SampleType(program)),
						   nsamps: uint64, burnin: uint64, lag: uint64, outstream: &C.FILE)
			var verbose = (outstream ~= nil)
			var iters = burnin + (nsamps*lag)
			var t0 = 0.0
			var currTrace = TraceType.salloc():init()
			var k = KernelType.salloc():init()
			for i=0,iters do
				if verbose then
					C.fprintf(C.getstderr(), " Iteration %u/%u\r", i+1, iters)
					C.flushstderr()
					if i == 1 then t0 = util.currentTimeInSeconds() end		-- Skip any one-time JIT costs
				end
				k:next(currTrace, i, iters)
				if i >= burnin and i % lag == 0 then
					samples:insert()
					var s = samples:get(samples:size()-1)
					S.copy(s.value, currTrace.returnValue)
					s.logprob = currTrace.logprob
					s.loglikelihood = 0.0 	-- Since MCMC already draws samples according to prior*likelihood
				end
			end
			var t1 = util.currentTimeInSeconds()
			if verbose then
				C.fprintf(C.getstderr(), "\n")
				escape
					if KernelType:getmethod("proposalsMade") and KernelType:getmethod("proposalsAccepted") then
						emit quote
							var pm = k:proposalsMade()
							var pa = k:proposalsAccepted()
							C.fprintf(outstream, "Acceptance Ratio: %u/%u (%g%%)\n", pa, pm, 100.0*double(pa)/pm)
						end
					end
					if KernelType:getmethod("printStats") then
						emit quote k:printStats(outstream) end
					end
				end
				C.fprintf(outstream, "Time: %g\n", t1 - t0)
			end
		end

		-- Return a macro if any params are quotes, otherwise return a Terra function
		local anyquotes = false
		for _,v in pairs(params) do anyquotes = anyquotes or terralib:isquote(v) end
		if anyquotes then
			return macro(function(samples)
				return `doMCMC(samples, _numsamps, _burnin, _lag, [&C.FILE](_verbose))
			end)
		else
			return terra(samples: &S.Vector(infer.SampleType(program)))
				return doMCMC(samples, _numsamps, _burnin, _lag, [&C.FILE](_verbose))
			end
		end
	end
end




-----------------------------------------
--      Some commonly-used kernels     --
-----------------------------------------


-- A metatype that adds fields/methods for recording/reporting proposal stats
local function KernelPropStats(T)
	T.entries:insert({field=propsMade, type=uint64})
	T.entries:insert({field=propsAccepted, type=uint64})
	terra T:proposalsMade() return self.propsMade end
	terra T:proposalsAccepted() return self.propsAccepted end
end


-- Performs the trace-MH algorithm from the lightweight implementation paper.
-- (Picks a random choice and proposes a change)
-- Can optionally specify whether the kernel should apply just to structural choices,
--    just to nonstructural choices, or to both (defaults to both).
-- NOTE: For this kernel, I've decided that the params should only be constants. It simplifies
--    some logic, and I really can't imagine a context where you'd want to determine them at runtime.
local function TraceMHKernel(params)
	params = params or {}
	local doStruct = params.doStruct
	local doNonstruct = params.doNonstruct
	if doStruct == nil then doStruct = true end
	if doNonstruct == nil then doNonstruct = true end
	assert(doStruct or doNonstruct, "TraceMHKernel: cannot set both 'doStruct' and 'doNonstruct' to false")
	assert(type(doStruct) == "boolean" and type(doNonstruct) == "boolean",
		"TraceMHKernel: 'doStruct' and 'doNonstruct' must be boolean constants")

	local filter = {}
	local oneTypeOnly = (doStruct and not doNonstruct) or (doNonstruct and not doStruct)
	if oneTypeOnly then
		filter = {isStructural=doStruct}
	end

	return function(TraceType)
		
		local struct TraceMHKernel(S.Object) {}
		KernelPropStats(TraceMHKernel)

		terra TraceMHKernel:__init()
			self.propsMade = 0
			self.propsAccepted = 0
		end

		terra TraceMHKernel:next(currTrace: &TraceType, iter: uint, numiters: uint)
			self.propsMade = self.propsMade + 1
			-- Make a copy of the current trace which we use to evaluate our proposal
			var nextTrace = TraceType.salloc():copy(currTrace)
			-- Pick a random choice uniformly at random
			var numchoices = [TraceType.countChoices(filter)](nextTrace)
			var randindex = random.random() * numchoices
			var rc = [TraceType.getChoice(filter)](nextTrace, randindex)
			-- Propose a change to that random choice and re-execute the program
			var fwdPropLP, rvsPropLP = rc:proposal()
			nextTrace:update(rc:isStructural())
			-- Account for probability changes due to dimension-jumping
			if rc:isStructural() then
				var oldnumchoices = numchoices
				var newnumchoices = [TraceType.countChoices(filter)](nextTrace)
				fwdPropLP = fwdPropLP + nextTrace.newlogprob - tmath.log(double(oldnumchoices))
				rvsPropLP = rvsPropLP + nextTrace.oldlogprob - tmath.log(double(newnumchoices))
			end
			-- Determine acceptance/rejection
			var acceptThresh = (nextTrace.logprob - currTrace.logprob) + rvsPropLP - fwdPropLP
			if nextTrace.conditionsSatisfied and tmath.log(random.random()) < acceptThresh then
				-- Accept
				self.propsAccepted = self.propsAccepted + 1
				util.swap(@currTrace, @nextTrace)
			end
		end

		return TraceMHKernel
	end
end


-- Stochastically chooses between multiple different MCMC kernels
-- The kernel selection weights can be constants or quotes
-- Both are collected in Lua tables.
local function MixtureKernel(kernels, weights)
	assert(#kernels == #weights,
		"MixtureKernel: number of kernels must equal number of weights")
	kernels = terralib.newlist(kernels)
	weights = terralib.newlist(weights)
	return function(TraceType)
		local skernels = kernels:map(function(k) return k(TraceType) end)
		
		local struct MixtureKernel(S.Object)
		{
			weights: qs.primfloat[ #weights ]
		}
		-- Insert an entry for every sub-kernel
		local function kernelEntry(i) return string.format("kernel%d", i-1) end
		for i,k in ipairs(skernels) do
			MixtureKernel.entries:insert({field=kernelEntry(i), type=k})
		end

		local weightsyms = weights:map(function(w) return symbol(qs.primfloat) end)
		terra MixtureKernel:__doinit([weightsyms])
			self:initmembers()
			escape
				for i,w in ipairs(weightsyms) do
					emit quote self.weights[ [i-1] ] = [w] end
				end
			end
		end

		MixtureKernel.methods.__init = macro(function(self)
			return `self:__doinit([weights])
		end)

		terra MixtureKernel:proposalsMade()
			var total : uint64 = 0
			escape
				for i,k in ipairs(skernels) do
					if k:getmethod("proposalsMade") then
						emit quote total = total + self.[kernelEntry(i)]:proposalsMade() end
					end
				end
			end
			return total
		end

		terra MixtureKernel:proposalsAccepted()
			var total : uint64 = 0
			escape
				for i,k in ipairs(skernels) do
					if k:getmethod("proposalsAccepted") then
						emit quote total = total + self.[kernelEntry(i)]:proposalsAccepted() end
					end
				end
			end
			return total
		end

		-- Print out acceptance ratio for all sub-kernels
		terra MixtureKernel:printStats(outstream: &C.FILE)
			escape
				for i,k in ipairs(skernels) do
					if k:getmethod("proposalsMade") and k:getmethod("proposalsAccepted") then
						emit quote
							var pm = self.[kernelEntry(i)]:proposalsMade()
							var pa = self.[kernelEntry(i)]:proposalsAccepted()
							C.fprintf(outstream, "   %s Acceptance Ratio: %u/%u (%g%%)\n", [tostring(k)], pa, pm, 100.0*double(pa)/pm)
						end
					end
					if k:getmethod("printStats") then
						emit quote
							self.[kernelEntry(i)]:printStats(outstream)
						end
					end
				end
			end
		end

		terra MixtureKernel:next(currTrace: &TraceType, iter: uint, numiters: uint)
			var randindex = [distrib.multinomial_array(#weights)(qs.primfloat)].sample(self.weights)
			escape
				for i,k in ipairs(skernels) do
					emit quote
						if randindex == [i-1] then
							self.[kernelEntry(i)]:next(currTrace, iter, numiters)
							return
						end
					end
				end
			end
		end

		return MixtureKernel
	end
end


-- Run the simulated annealing meta-inference algorithm on top of another MCMC kernel
-- 'annealSched' can be any Terra function or macro that takes in two ints (curr iteration
--     + total number of iterations) and returns a qs.primfloat
local function AnnealingKernel(kernel, annealSched)
	return function(TraceType)
		local skernel = kernel(TraceType)

		local struct AnnealingKernel(S.Object)
		{
			k: skernel
		}

		terra AnnealingKernel:next(currTrace: &TraceType, iter: uint, numiters: uint)
			currTrace:setTemperature(annealSched(iter, numiters))
			self.k:next(currTrace, iter, numiters)
		end

		-- Forward all other method calls (e.g. printStats) to inner kernel
		function AnnealingKernel.metamethods.__methodmissing(methodname, self, ...)
			local args = {...}
			return `self.k:[methodname]([args])
		end

		return AnnealingKernel
	end
end



-- TODO: DriftKernel?



return
{
	exports = 
	{
		MCMC = MCMC,
		KernelPropStats = KernelPropStats,
		TraceMHKernel = TraceMHKernel,
		MixtureKernel = MixtureKernel,
		AnnealingKernel = AnnealingKernel
	}
}




