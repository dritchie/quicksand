local util = terralib.require("qs.lib.util")

local S = util.require("lib.std")
local globals = util.require("globals")
local progmod = util.require("progmodule")
local trace = util.require("trace")
local random = util.require("lib.random")
local tmath = util.require("lib.tmath")

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
		local TraceType = trace.RandExecTrace(program, globals.primfloat)
		local KernelType = kernel(TraceType)

		local terra doMCMC(samples: &S.Vector(SampleType(program)),
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
				k:next(&currTrace)
				if i >= burnin and i % lag == 0 then
					samples:insert()
					var s = samples:get(samples:size()-1)
					S.copy(s.value, currTrace.returnValue)
					s.logprob = currTrace.logprob
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
							C.fprintf(outstream, "Acceptance ratio: %u/%u (%g\%)\n", pa, pm, 100.0*double(pa)/pm)
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
				return `doMCMC(samples, _numsamps, _burnin, _lag, _verbose)
			end)
		else
			return terra(samples: &S.Vector(SampleType(program)))
				return doMCMC(samples, _numsamps, _burnin, _lag, _verbose)
			end
		end
	end
end




-----------------------------------------
--      Some commonly-used kernels     --
-----------------------------------------


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
		
		local struct Kernel(S.Object)
		{
			propsMade: uint64,
			propsAccepted: uint64
		}

		terra Kernel:__init()
			self.propsMade = 0
			self.propsAccepted = 0
		end

		terra Kernel:proposalsMade() return self.propsMade end
		terra Kernel:proposalsAccepted() return self.proposalsAccepted end

		terra Kernel:next(currTrace: &TraceType)
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
			if nextTrace.newlogprob ~= 0.0 or nextTrace.oldlogprob ~= 0.0 then
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

		return Kernel
	end
end


-- TODO: MultiKernel, SchedulingKernel (, DriftKernel?)


return
{
	exports = 
	{
		MCMC = MCMC,
		TraceMHKernel = TraceMHKernel
	}
}




