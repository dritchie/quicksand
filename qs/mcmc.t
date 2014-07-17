local util = terralib.require("qs.lib.util")

local S = util.require("lib.std")
local globals = util.require("globals")
local progmod = util.require("progmodule")
local trace = util.require("trace")

local C = terralib.includecstring [[
#include <stdio.h>
inline FILE* getstderr() { return stderr; }
inline FILE* getstdout() { return stdout; }
inline void flushstderr() { fflush(stderr); }
]]


local function hasMember(typ, name)
	for _,e in ipairs(typ.entries) do
		if e.field == name then return true end
	end
	return false
end


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
--                  Will look for fields 'proposalsMade' and 'proposalsAccepted' in kernel type.
--					Will also look for method 'printStates' in kernel type.
local function MCMC(kernel, params)
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
					if hasMember(KernelType, "proposalsMade") and hasMember(KernelType, "proposalsAccepted") then
						emit quote C.fprintf(outstream, "Acceptance ratio: %u/%u (%g\%)\n",
							k.proposalsAccepted, k.proposalsMade, 100.0*double(k.proposalsAccepted)/k.proposalsMade)
						end
					end
					if KernelType:getmethod("printStats") then
						emit quote k:printStats(outstream) end
					end
				end
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




return
{
	exports = 
	{
		MCMC = MCMC
	}
}




