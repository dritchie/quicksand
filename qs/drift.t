local util = terralib.require("qs.lib.util")

local S = util.require("lib.std")
local qs = util.require("globals")
local mcmc = util.require("mcmc")
local distrib = util.require("distrib")
local tmath = util.require("lib.tmath")
local random = util.require("lib.random")


-- An MCMC kernel that performs Random Walk Metropolis via gaussian drift.
-- params are:
--    * scale: bandwidth to use for gaussian proposals (defaults to 1.0)
--    * doScaleAdapt: Automatically adapt scale param (defaults to true)
--    * lexicalScaleSharing: If true, all choices from the same source code location
--         will share one adapted proposal bandwidth. If false, every choice will
--         will have its own adapted proposal bandwidth (defaults to true).
local function DriftKernel(params)
	params = params or {}
	local scale = params.scale or 1.0
	local doScaleAdapt = true
	if params.doScaleAdapt ~= nil then
		doScaleAdapt = params.doScaleAdapt
	end
	local lexicalScaleSharing = true
	if params.lexicalScaleSharing ~= nil then
		lexicalScaleSharing = params.lexicalScaleSharing
	end

	local lerp = macro(function(a, b, t)
		return `(1.0-t)*a + t*b
	end)

	return function(TraceType)
		
		local struct DriftKernel(S.Object)
		{
			scale: qs.float,
			adapting: bool,

			realcomps: S.Vector(qs.float),
			realcomps_scratch: S.Vector(qs.float),

			lastTraceSeen: &TraceType,
			lastNumUpdatesSeen: int64
		}
		mcmc.KernelPropStats(DriftKernel)

		terra DriftKernel:__doinit(scale: qs.float, doAdapt: bool)
			self:initmembers()

			self.scale = scale
			self.adapting = doAdapt

			self.lastTraceSeen = nil
			self.lastNumUpdatesSeen = -1

			self:initKernelPropStats()
		end

		DriftKernel.methods.__init = macro(function(self)
			return `self:__doinit(scale, doScaleAdapt)
		end)

		terra DriftKernel:next(currTrace: &TraceType, iter: uint, numiters: uint)
			self.propsMade = self.propsMade + 1
			-- Check if the trace has been interfered with since last :next call
			self:checkForChanges(currTrace)

			var n = self.realcomps:size()
			-- For each component, sample a gaussian perturbation
			for i=0,n do
				self.realcomps_scratch(i) = [distrib.gaussian(qs.float)].sample(self.realcomps(i), self.scale)
			end

			-- Create a scratch trace, copy these components back into it, and update.
			var nextTrace = TraceType.salloc():copy(currTrace)
			var index = 0ULL
			var numvars = [TraceType.countChoices({isStructural=false})](nextTrace)
			for i=0,numvars do
				var rc = [TraceType.getChoice({isStructural=false})](nextTrace, i)
				index = index + rc:setUnboundedRealComps(&self.realcomps_scratch, index)
			end
			nextTrace:update(false)
			
			-- Accept/reject
			var targetAcceptRatio = 0.234
			if n < 5 then targetAcceptRatio = lerp(0.44, 0.234, (n-1)/4.0) end
			var acceptThresh = nextTrace.logprob - currTrace.logprob
			if nextTrace.conditionsSatisfied and tmath.log(random.random()) < acceptThresh then
				self.propsAccepted = self.propsAccepted + 1
				util.swap(@currTrace, @nextTrace)
				util.swap(self.realcomps, self.realcomps_scratch)
			end

			-- Record the new last-seen stats
			self.lastTraceSeen = currTrace
			self.lastNumUpdatesSeen = currTrace.numUpdates
		end

		terra DriftKernel:checkForChanges(currTrace: &TraceType)
			-- If the trace has been interfered with since the last run of this kernel, then we
			--    need to fetch the real components back from the trace
			if currTrace ~= self.lastTraceSeen or currTrace.numUpdates ~= self.lastNumUpdatesSeen then
				self.realcomps:clear()
				var numvars = [TraceType.countChoices({isStructural=false})](currTrace)
				for i=0,numvars do
					var rc = [TraceType.getChoice({isStructural=false})](currTrace, i)
					rc:getUnboundedRealComps(&self.realcomps)
				end
				var n = self.realcomps:size()
				self.realcomps_scratch:clear()
				for i=0,n do self.realcomps_scratch:insert() end
			end
		end

		return DriftKernel

	end
end



return
{
	exports = 
	{
		DriftKernel = DriftKernel
	}	
}






