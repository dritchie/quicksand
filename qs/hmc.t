local util = terralib.require("qs.lib.util")

local S = util.require("lib.std")
local qs = util.require("globals")
local mcmc = util.require("mcmc")
local distrib = util.require("distrib")
local tmath = util.require("lib.tmath")
local random = util.require("lib.random")
local larj = util.require("larj")


-- Univariate dual-averaging optimization (for HMC step size adaptation)
-- Adapted from Stan
local struct DualAverage(S.Object)
{
	gbar: qs.float,
	xbar: qs.float,
	x0: qs.float,
	lastx: qs.float,
	k: uint,
	gamma: qs.float
}

terra DualAverage:reinit(x0: qs.float, gamma: qs.float)
	self.k = 0
	self.x0 = x0
	self.lastx = x0
	self.gbar = 0.0
	self.xbar = 0.0
	self.gamma = gamma
end

-- So that we can use it with initmembers
terra DualAverage:__init()
	self:reinit(0.0, 0.0)
end

terra DualAverage:update(g: qs.float)
	self.k = self.k + 1
	var avgeta = 1.0 / (self.k + 10)
	var xbar_avgeta = tmath.pow(qs.float(self.k), -0.75)
	var muk = 0.5 * tmath.sqrt(qs.float(self.k)) / self.gamma
	self.gbar = avgeta*g + (1-avgeta)*self.gbar
	self.lastx = self.x0 - muk*self.gbar
	var oldxbar = self.xbar
	self.xbar = xbar_avgeta*self.lastx + (1-xbar_avgeta)*self.xbar
	return self.lastx
end


-- An MCMC kernel that does Hamiltonian Monte Carlo.
-- params are:
--    * stepSize: size of each leapfrog step in a proposal trajectory. If step size
--         adaptation is requested, this is the initial step size. Defaults to 1.0
--    * numSteps: number of steps in a proposal trajectory. Defaults to 1 (i.e. Langevin Monte Carlo)
--    * doStepSizeAdapt: whether to automatically adapt the step size. Defaults to true.
local function HMCKernel(params)
	params = params or {}
	local stepSize = params.stepSize or 1.0
	local numSteps = params.numSteps or 1
	local doStepSizeAdapt = true
	if params.doStepSizeAdapt ~= nil then doStepSizeAdapt = params.doStepSizeAdapt end

	local adaptRate = 0.05
	local targetAcceptRate_LMC = 0.574
	local targetAcceptRate_HMC = 0.65

	return function(TraceType)

		local DualTraceType = TraceType.withRealType(qs.dualnum)

		local isDoingLARJAnnealing = larj.isInterpolationTrace(TraceType)

		local veccopy = macro(function(dst, src)
			return quote
				var n = src:size()
				dst:clear(); dst:reserve(n)
				for i=0,n do dst:insert(src(i)) end
			end
		end)
		
		local struct HMCKernel(S.Object)
		{
			stepSize: qs.float,
			numSteps: uint64,
			adapting: bool,
			adapter: DualAverage,
			targetAcceptRate: qs.float,

			dualTrace: DualTraceType,
			lastTraceSeen: &TraceType,
			lastNumUpdatesSeen: int64,

			positions: S.Vector(qs.float),
			positions_scratch: S.Vector(qs.float),
			positions_dual_scratch: S.Vector(qs.dualnum),
			gradient: S.Vector(qs.float),
			gradient_scratch: S.Vector(qs.float),
			momenta: S.Vector(qs.float),
			invMasses: S.Vector(qs.float)
		}
		mcmc.KernelPropStats(HMCKernel)

		-- We need a couple of extra members if we're doing LARJ annealing
		if isDoingLARJAnnealing then
			HMCKernel.entries:insert({field="larjOldCompIndices", type=S.Vector(uint64)})
			HMCKernel.entries:insert({field="larjNewCompIndices", type=S.Vector(uint64)})
		end

		terra HMCKernel:__doinit(stepSize: qs.float, numSteps: uint64, doStepSizeAdapt: bool)
			-- Will cause self.dualTrace to initialize, which is not strictly necessary, but it's
			--    easier to just let this happen then to try and work around it.
			self:initmembers()

			self.stepSize = stepSize
			self.numSteps = numSteps
			self.adapting = doStepSizeAdapt
			if numSteps == 1 then
				self.targetAcceptRate = targetAcceptRate_LMC
			else
				self.targetAcceptRate = targetAcceptRate_HMC
			end

			self.lastTraceSeen = nil
			self.lastNumUpdatesSeen = -1

			self.propsMade = 0
			self.propsAccepted = 0
		end

		HMCKernel.methods.__init = macro(function(self)
			return `self:__doinit(stepSize, numSteps, doStepSizeAdapt)
		end)

		terra HMCKernel:next(currTrace: &TraceType, iter: uint, numiters: uint)
			self.propsMade = self.propsMade + 1

			-- If trace has changed or been interfered with, recopy stuff
			self:checkForChanges(currTrace)

			-- Sample momentum variables
			self:sampleMomenta()

			-- Compute initial Hamiltonian
			var H = self:hamiltonian(currTrace.logprob)

			-- Simulate a Hamiltonian dynamics trajectory
			var newlp : qs.float
			veccopy(&self.positions_scratch, &self.positions)
			veccopy(&self.gradient_scratch, &self.gradient)
			for i=0,self.numSteps do
				newlp = self:step(&self.positions_scratch, &self.gradient_scratch)
			end

			-- Compute final Hamiltonian
			var H_new = self:hamiltonian(newlp)
			var dH = H_new - H

			-- Update step size, if we're doing adaptation
			if self.adapting then
				self:adaptStepSize(dH)
			end

			-- Accept/reject decision
			var accept = self.dualTrace.conditionsSatisfied and tmath.log(random.random()) < dH
			if accept then
				self.propsAccepted = self.propsAccepted + 1
				util.swap(self.positions, self.positions_scratch)
				util.swap(self.gradient, self.gradient_scratch)
				-- Update currTrace with these new values, run :update to flush them
				--    through the program.
				var index = 0ULL
				var numvars = [TraceType.countChoices({isStructural=false})](currTrace)
				for i=0,numvars do
					var rc = [TraceType.getChoice({isStructural=false})](currTrace, i)
					index = index + rc:setUnboundedRealComps(&self.positions, index)
				end
				-- Turn factor/condition eval off, write to logprob/loglikelihood manually
				-- (Saves unnecessary computation of evaluating expensive factors/conditions)
				currTrace:update(false, false)
				currTrace:copyProbabilities(self.dualTrace)
			end

			-- Record the new last-seen stats
			self.lastTraceSeen = currTrace
			self.lastNumUpdatesSeen = currTrace.numUpdates
		end

		terra HMCKernel:checkForChanges(currTrace: &TraceType)
			-- If currTrace is the same as the last trace we've seen AND no other kernel
			--    has updated it since then, we don't have to do anything. Otherwise, stuff.
			if currTrace ~= self.lastTraceSeen or currTrace.numUpdates ~= self.lastNumUpdatesSeen then

				-- Grab all the real components from the trace, as any of them may have changed
				var oldn = self.positions:size()
				self.positions:clear()
				var newn = 0U
				var numvars = [TraceType.countChoices({isStructural=false})](currTrace)
				var numCompsPerChoice = [S.Vector(uint64)].salloc():init()
				for i=0,numvars do
					var rc = [TraceType.getChoice({isStructural=false})](currTrace, i)
					var n = rc:getUnboundedRealComps(&self.positions)
					newn = newn + n
					numCompsPerChoice:insert(n)
				end
				if newn == 0 then
					S.printf("Cannot use HMC on a program with no real-valued non-structural random choices.\n")
					S.assert(false)
				end

				-- If the number of real components has changed since our last iteration, then:
				if newn ~= oldn then
					-- We need to rebuild our internal dual trace.
					self.dualTrace:destruct()
					[DualTraceType.copyFromRealType(qs.float)](&self.dualTrace, currTrace)

					-- We also need to re-size and re-initialize the inverse masses.
					self.invMasses:clear(); self.invMasses:reserve(newn)
					for i=0,newn do self.invMasses:insert(1.0) end

					-- If we're doing LARJ, then we need to record which position variables are
					--    annealing out / annealing in.
					escape
						if isDoingLARJAnnealing then
							emit quote
								self.larjOldCompIndices:clear()
								self.larjNewCompIndices:clear()
								var currCompIndex = 0
								for i=0,numvars do
									var n = numCompsPerChoice(i)
									var rc = [TraceType.getChoice({isStructural=false})](currTrace, i)
									if rc:isAnnealingOut() then
										for j=0,n do
											self.larjOldCompIndices:insert(currCompIndex+j)
										end
									elseif rc:isAnnealingIn() then
										for j=0,n do
											self.larjNewCompIndices:insert(currCompIndex+j)
											-- Also initialize the inverse masses for annealing in variables to zero
											self.invMasses(currCompIndex+j) = 0.0
										end
									end
									currCompIndex = currCompIndex + n
								end
							end
						end
					end
				end

				-- Initialize the gradient
				self:traceUpdate(&self.positions, &self.gradient)

				-- If this is the very first application of the kernel and we have adaptation on, then:
				if newn ~= oldn and self.adapting and self.lastTraceSeen == nil then
					-- Search for a decent initial step size
					self:searchForInitialStepSize()
					-- Set up the adapter
					self.adapter:reinit(self.stepSize, adaptRate)
				end
			end

			-- Always check for temperature changes
			self.dualTrace:setTemperature(currTrace.temperature)

			-- If we're doing LARJ, then we always need to update things according to the current interpolation
			--    alpha (e.g. prevent variables that are annealing out from going crazy because they have little
			--    effect on the log probability)
			escape
				if isDoingLARJAnnealing then
					emit quote
						self.dualTrace.alpha = currTrace.alpha
						var oldScale = 1.0 - currTrace.alpha
						var newScale = currTrace.alpha
						for oldi in self.larjOldCompIndices do
							self.invMasses(oldi) = oldScale
						end
						for newi in self.larjNewCompIndices do
							self.invMasses(newi) = newScale
						end
					end
				end
			end

		end

		terra HMCKernel:sampleMomenta()
			var n = self.positions:size()
			self.momenta:clear(); self.momenta:reserve(n)
			for i=0,n do
				self.momenta:insert([distrib.gaussian(qs.float)].sample(0.0, 1.0) * self.invMasses(i))
			end
		end

		terra HMCKernel:hamiltonian(logprob: qs.float)
			var kinetic = 0.0
			for i=0,self.momenta:size() do
				var m = self.momenta(i)
				kinetic = kinetic + m*m*self.invMasses(i)
			end
			kinetic = -0.5*kinetic
			return kinetic + logprob
		end

		terra HMCKernel:step(pos: &S.Vector(qs.float), grad: &S.Vector(qs.float))
			-- Momentum half-update
			for i=0,self.momenta:size() do
				self.momenta(i) = self.momenta(i) + 0.5*self.stepSize*grad(i)
			end
			-- Position update
			for i=0,pos:size() do
				pos(i) = pos(i) + self.stepSize*self.momenta(i)*self.invMasses(i)
			end
			-- Compute gradient at new position
			var lp = self:traceUpdate(pos, grad)
			-- Momentum half-update
			for i=0,self.momenta:size() do
				self.momenta(i) = self.momenta(i) + 0.5*self.stepSize*grad(i)
			end
			return lp
		end

		terra HMCKernel:traceUpdate(pos: &S.Vector(qs.float), grad: &S.Vector(qs.float))
			-- Convert pos into dual numbers
			veccopy(&self.positions_dual_scratch, pos)
			-- Copy dual-converted pos back into the trace
			var index = 0ULL
			var numvars = [DualTraceType.countChoices({isStructural=false})](&self.dualTrace)
			for i=0,numvars do
				var rc = [DualTraceType.getChoice({isStructural=false})](&self.dualTrace, i)
				index = index + rc:setStoredRealComps(&self.positions_dual_scratch, index)
			end
			-- Update the trace
			self.dualTrace:update(false)
			var duallp = self.dualTrace.logprob
			var lp = duallp:val()
			-- Compute new gradient
			duallp:grad(&self.positions_dual_scratch, grad)
			return lp
		end

		-- Code adapted from Stan
		terra HMCKernel:searchForInitialStepSize()
			veccopy(&self.positions_scratch, &self.positions)
			veccopy(&self.gradient_scratch, &self.gradient)
			self:sampleMomenta()
			var lastlp = self.dualTrace.logprob:val()
			var lp = self:step(&self.positions_scratch, &self.gradient_scratch)
			var H = lp - lastlp
			var direction = -1
			if H > tmath.log(0.5) then direction = 1 end
			while true do
				veccopy(&self.positions_scratch, &self.positions)
				veccopy(&self.gradient_scratch, &self.gradient)
				self:sampleMomenta()
				var lastlp = self.dualTrace.logprob:val()
				var lp = self:step(&self.positions_scratch, &self.gradient_scratch)
				var H = lp - lastlp
				-- Fail if the latest step made the lp NaN
				if not (lp == lp) then
					S.printf("HMC step size search failed because logprob became NaN\n")
					S.assert(false)
				end
				-- If our initial step improved the posterior by more than 0.5, then
				--    keep doubling step size until the initial step improves by as
				--    close as possible to 0.5
				-- If our initial step improved the posterior by less than 0.5, then
				--    keep halving the step size until the initial step improves by
				--    as close as possible to 0.5
				if (direction == 1) and (H <= tmath.log(0.5)) then
					break
				elseif (direction == -1) and (H >= tmath.log(0.5)) then
					break
				elseif direction == 1 then
					self.stepSize = self.stepSize * 2.0
				else
					self.stepSize = self.stepSize * 0.5
				end
				-- Check for divergence to infinity or collapse to zero
				if self.stepSize > 1e300 then
					S.printf("HMC step size search diverged to infinity (Is your probability distribution flat?)\n")
					S.assert(false)
				end
				if self.stepSize == 0 then
					S.printf("HMC step size search collapsed to zero (Is your probability distribution discontinuous?)\n")
					S.assert(false)
				end
			end
		end

		-- Code adapted from Stan
		terra HMCKernel:adaptStepSize(dH: qs.float)
			var EdH = tmath.exp(dH)
			if EdH > 1.0 then EdH = 1.0 end
			-- Supress NaNs
			if not (EdH == EdH) then EdH = 0.0 end
			var adaptGrad = self.targetAcceptRate - EdH
			-- Dual averaging
			self.stepSize = tmath.exp(self.adapter:update(adaptGrad))
		end

		return HMCKernel
	end
end



return
{
	exports =
	{
		HMCKernel = HMCKernel
	}
}





