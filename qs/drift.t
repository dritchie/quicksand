local util = terralib.require("qs.lib.util")

local S = util.require("lib.std")
local qs = util.require("globals")
local mcmc = util.require("mcmc")
local distrib = util.require("distrib")
local tmath = util.require("lib.tmath")
local random = util.require("lib.random")



-- Compute a running estimate of variance
-- Based on code from: http://www.johndcook.com/standard_deviation.html
local struct RunningVar(S.Object)
{
	mean: qs.float,
	variance: qs.float,
	n: uint
}

terra RunningVar:__init()
	self.n = 0
end

terra RunningVar:update(x: qs.float)
	self.n = self.n + 1
	if self.n == 1 then
		self.mean = x
		self.variance = 0.0
	else
		var newmean = self.mean + (x - self.mean)/self.n
		self.variance = self.variance + (x - self.mean)*(x - newmean)
		self.mean = newmean
		-- S.assert(self.mean == self.mean)
		-- S.assert(self.variance == self.variance)
	end
end

terra RunningVar:getN() return self.n end

terra RunningVar:getVariance()
	if self.n <= 1 then
		return 0.0
	else
		return self.variance/(self.n - 1)
	end
end

terra RunningVar:getStdDev() return tmath.sqrt(self:getVariance()) end



-- An MCMC kernel that performs Random Walk Metropolis via gaussian drift.
-- params are:
--    * scale: bandwidth to use for gaussian proposals (defaults to 1.0)
--    * doScaleAdapt: Automatically adapt proposal scales (defaults to true)
--    * lexicalScaleSharing: If true, all choices from the same source code location
--         will share one adapted proposal bandwidth. If false, every choice will
--         will have its own adapted proposal bandwidth (defaults to false).
-- Based loosely on .mcmcam from LaplacesDemon (https://github.com/Statisticat/LaplacesDemon)
--    (This version only adapts a diagonal covariance matrix)
local function DriftKernel(params)
	params = params or {}
	local scale = params.scale or 1.0
	local doScaleAdapt = true
	if params.doScaleAdapt ~= nil then
		doScaleAdapt = params.doScaleAdapt
	end
	local lexicalScaleSharing = false
	if params.lexicalScaleSharing ~= nil then
		lexicalScaleSharing = params.lexicalScaleSharing
	end

	local lerp = macro(function(a, b, t)
		return `(1.0-t)*a + t*b
	end)

	return function(TraceType)
		
		local struct DriftKernel(S.Object)
		{
			initScale: qs.float,
			scales: S.Vector(qs.float),
			varEsts: S.Vector(RunningVar),
			adapting: bool,

			realcomps: S.Vector(qs.float),
			realcomps_scratch: S.Vector(qs.float),

			lastTraceSeen: &TraceType,
			lastNumUpdatesSeen: int64
		}
		mcmc.KernelPropStats(DriftKernel)

		-- Maps component index to scale index (constructed using random choice lexical IDs)
		if lexicalScaleSharing then
			DriftKernel.entries:insert({field="scaleIndexForComp", type=S.Vector(uint)})
		end

		-- Retrieve the scale index for a give component index
		DriftKernel.methods.scaleIndex = macro(function(self, i)
			if lexicalScaleSharing then
				return `self.scaleIndexForComp(i)
			else
				return i
			end
		end)

		terra DriftKernel:__doinit(scale: qs.float, doAdapt: bool)
			self:initmembers()

			self.initScale = scale
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
				self.realcomps_scratch(i) = [distrib.gaussian(qs.float)].sample(self.realcomps(i), self.scales(self:scaleIndex(i)))
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

			-- Adaptation
			if self.adapting then
				self:adapt()
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
				-- Also may need to rebuild the scale index map, if we're doing lexical scale sharing
				var maxlexid = 0U
				escape
					if lexicalScaleSharing then
						emit quote
							self.scaleIndexForComp:clear()
						end
					end
				end
				var numvars = [TraceType.countChoices({isStructural=false})](currTrace)
				if numvars == 0 then
					S.printf("DriftKernel: found 0 non-structural random choices\n")
					S.assert(false)
				end
				for i=0,numvars do
					var rc = [TraceType.getChoice({isStructural=false})](currTrace, i)
					var ncomps = rc:getUnboundedRealComps(&self.realcomps)
					escape
						if lexicalScaleSharing then
							emit quote
								var lexid = rc:getLexicalID()
								if lexid > maxlexid then maxlexid = lexid end
								for j=0,ncomps do self.scaleIndexForComp:insert(lexid) end
							end
						end
					end
				end
				var n = self.realcomps:size()
				if n == 0 then
					S.printf("DriftKernel: found non-structural random choices, but none of them are real-valued.\n")
					S.assert(false)
				end
				self.realcomps_scratch:clear()
				for i=0,n do self.realcomps_scratch:insert() end

				var newScalesSize = [lexicalScaleSharing and (`maxlexid+1) or n]

				-- Also may need to expand scales vector and corresponding variance estimators
				-- (Note that scales never shrinks, it only expands. This way when we go from a big
				--     structure to a small one and back again, we don't throw away any adaptation)
				var oldScalesSize = self.scales:size()
				for i=oldScalesSize,newScalesSize do
					self.scales:insert(self.initScale)
					var vest = self.varEsts:insert()
					vest:init()
				end
			end
		end

		-- There are some magic numbers in here...
		terra DriftKernel:adapt()
			var ratio = qs.float(self.propsAccepted)/self.propsMade
			for i=0,self.realcomps:size() do 
				var si = self:scaleIndex(i)
				-- Update our running estimates of variance (but only if our kernel
				--    is doing minimally well enough that we can trust the samples)
				if ratio >= 0.05 and self.propsAccepted > 5 then
					self.varEsts(si):update(self.realcomps(i))
				end
			end
			for si=0,self.scales:size() do
				-- Update scales based on these variance estimates (but only if the
				--    estimators have collected enough samples)
				if self.varEsts(si):getN() > 100 then
					self.scales(si) = self.varEsts(si):getStdDev()
				end

				-- If we're not doing well enough to collect samples, then we should
				--    uniformly scale down the proposal scales to lead to a higher
				--    acceptance ratio
				if ratio < 0.05 and self.propsAccepted > 5 then
					self.scales(si) = self.scales(si) * 0.99
				end
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







