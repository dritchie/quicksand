local util = require("qs.lib.util")

local S = require("qs.lib.std")
local qs = require("qs.globals")
local mcmc = require("qs.mcmc")
local distrib = require("qs.distrib")
local tmath = require("qs.lib.tmath")
local random = require("qs.lib.random")
local HashMap = require("qs.lib.hashmap")



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

		local struct ScaleAdapter(S.Object)
		{
			scale: qs.float,
			varEst: RunningVar
		}
		
		local struct DriftKernel(S.Object)
		{
			initScale: qs.float,
			scaleGroups: S.Vector(S.Vector(ScaleAdapter)),
			adapting: bool,

			realcomps: S.Vector(qs.float),
			realcomps_scratch: S.Vector(qs.float),

			lastTraceSeen: &TraceType,
			lastNumUpdatesSeen: int64
		}
		mcmc.KernelPropStats(DriftKernel)

		-- Mapping component index to scale group index (constructed using random choice lexical IDs)
		-- Also mapping component index to index within scale group
		if lexicalScaleSharing then
			DriftKernel.entries:insert({field="scaleGroupIndexForComp", type=S.Vector(uint)})
			DriftKernel.entries:insert({field="indexWithinScaleGroupForComp", type=S.Vector(uint)})
		end

		-- Retrieve the scale index for a give component index
		DriftKernel.methods.getScale = macro(function(self, i)
			if lexicalScaleSharing then
				return `&self.scaleGroups(self.scaleGroupIndexForComp(i))(self.indexWithinScaleGroupForComp(i))
			else
				return `&self.scaleGroups(0)(i)
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
				self.realcomps_scratch(i) = [distrib.gaussian(qs.float)].sample(self.realcomps(i), self:getScale(i).scale)
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
			--    need to fetch the real components back from the trace, as well as update/rebuild any data
			--    structures used for adaptation.
			if currTrace ~= self.lastTraceSeen or currTrace.numUpdates ~= self.lastNumUpdatesSeen then
				self.realcomps:clear()
				var nCompsPerScaleGroup = [S.Vector(uint)].salloc():init()
				var lexidToGroupIndex = [HashMap(uint,uint)].salloc():init()
				escape
					if lexicalScaleSharing then
						-- Initialize vectors used to map component ids to appropriate scales.
						emit quote
							self.scaleGroupIndexForComp:clear()
							self.indexWithinScaleGroupForComp:clear()
						end
					else
						-- There's just one scale group, so go ahead and initialize it, if it hasn't
					    --    been initialized yet.
						emit quote
							if self.scaleGroups:size() == 0 then
								self.scaleGroups:insert():init()
							end
						end
					end
				end
				-- Fetch all the real components from the trace
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
								var groupIndexPtr, foundit = lexidToGroupIndex:getOrCreatePointer(lexid)
								if not foundit then
									@groupIndexPtr = nCompsPerScaleGroup:size()
									nCompsPerScaleGroup:insert(0)
									-- Initialize the scale group if needed.
									if self.scaleGroups:size() < @groupIndexPtr+1 then
										var oldsize = self.scaleGroups:size()
										for i=oldsize,@groupIndexPtr+1 do
											self.scaleGroups:insert():init()
										end
									end 
								end
								-- Fill in the vectors used to map component ids to appropriate scales.
								for j=0,ncomps do
									self.scaleGroupIndexForComp:insert(@groupIndexPtr)
									self.indexWithinScaleGroupForComp:insert(j)
								end
								-- Keep track of the maximum number of components per scale group
								if ncomps > nCompsPerScaleGroup(@groupIndexPtr) then
									nCompsPerScaleGroup(@groupIndexPtr) = ncomps
								end
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

				-- S.printf("\n-----\n")
				-- S.printf("num groups: %u\n", nCompsPerScaleGroup:size())
				-- for i=0,nCompsPerScaleGroup:size() do
				-- 	S.printf("%u  ", nCompsPerScaleGroup(i))
				-- end
				-- S.printf("\n")

				-- If we're not doing lexical scale sharing, then all scales are concentrated in one group
				escape
					if not lexicalScaleSharing then
						emit quote nCompsPerScaleGroup:insert(n) end
					end
				end

				-- Expand any scale groups if they aren't yet big enough to accommodate all the components
				--    we found.
				-- S.printf("scaleGroups:size(): %u\n", self.scaleGroups:size())
				for i=0,nCompsPerScaleGroup:size() do
					var oldsize = self.scaleGroups(i):size()
					-- S.printf("oldsize: %u, nCompsPerScaleGroup(i): %u\n", oldsize, nCompsPerScaleGroup(i))
					for j=oldsize,nCompsPerScaleGroup(i) do
						var scale = self.scaleGroups(i):insert():init()
						scale.scale = self.initScale
					end
				end
			end
		end

		-- There are some magic numbers in here...
		terra DriftKernel:adapt()
			var ratio = qs.float(self.propsAccepted)/self.propsMade
			for i=0,self.realcomps:size() do 
				var scale = self:getScale(i)
				-- Update our running estimates of variance (but only if our kernel
				--    is doing minimally well enough that we can trust the samples)
				if ratio >= 0.05 and self.propsAccepted > 5 then
					scale.varEst:update(self.realcomps(i))
				end
			end
			for scaleGroup in self.scaleGroups do
				for scale in scaleGroup do
					-- Update scales based on these variance estimates (but only if the
					--    estimators have collected enough samples)
					if scale.varEst:getN() > 100 then
						scale.scale = scale.varEst:getStdDev()
					end

					-- If we're not doing well enough to collect samples, then we should
					--    uniformly scale down the proposal scales to lead to a higher
					--    acceptance ratio
					if ratio < 0.05 and self.propsAccepted > 5 then
						scale.scale = scale.scale * 0.99
					end
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







