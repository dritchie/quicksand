local util = terralib.require("qs.lib.util")

local S = util.require("lib.std")
local qs = util.require("globals")
local mcmc = util.require("mcmc")
local distrib = util.require("distrib")
local tmath = util.require("lib.tmath")
local random = util.require("lib.random")


-- An MCMC kernel that performs Hit And Run Metroplis sampling
-- params are:
--    * scale: How big the proposed jumps should be (defaults to 1.0)
--    * doScaleAdapt: Automatically adapt scale param (defaults to true)
local function HARMKernel(params)
	params = params or {}
	local scale = params.scale or 1.0
	local doScaleAdapt = true
	if params.doScaleAdapt ~= nil then
		doScaleAdapt = params.doScaleAdapt
	end

	local lerp = macro(function(a, b, t)
		return `(1.0-t)*a + t*b
	end)

	return function(TraceType)
		
		local struct HARMKernel(S.Object)
		{
			scale: qs.float,
			adapting: bool,

			realcomps: S.Vector(qs.float),
			direction: S.Vector(qs.float)
		}
		mcmc.KernelPropStats(HARMKernel)

		terra HARMKernel:__doinit(scale: qs.float, doAdapt: bool)
			self:initmembers()

			self.scale = scale
			self.adapting = doAdapt

			self:initKernelPropStats()
		end

		HARMKernel.methods.__init = macro(function(self)
			return `self:__doinit(scale, doScaleAdapt)
		end)

		terra HARMKernel:next(currTrace: &TraceType, iter: uint, numiters: uint)
			self.propsMade = self.propsMade + 1
			-- Get the real components of the trace
			self.realcomps:clear()
			var numvars = [TraceType.countChoices({isStructural=false})](currTrace)
			for i=0,numvars do
				var rc = [TraceType.getChoice({isStructural=false})](currTrace, i)
				rc:getUnboundedRealComps(&self.realcomps)
			end
			var n = self.realcomps:size()
			-- Sample a random direction (random unit vector of dimension realcomps:size())
			-- Use propose a move along this direction, scaled by ~ uniform(0, self.scale)
			self.direction:clear(); self.direction:reserve(n)
			var norm = 0.0
			for i=0,n do
				var x = [distrib.gaussian(qs.float)].sample(0.0, 1.0)
				norm = norm + x*x
				self.direction:insert(x)
			end
			norm = tmath.sqrt(norm)
			var scale = random.random() * self.scale
			for i=0,n do
				self.direction(i) = self.direction(i)/norm
				self.realcomps(i) = self.realcomps(i) + (scale * self.direction(i))
			end
			-- Create a scratch trace, copy these components back into it, and update.
			var nextTrace = TraceType.salloc():copy(currTrace)
			var index = 0ULL
			for i=0,numvars do
				var rc = [TraceType.getChoice({isStructural=false})](nextTrace, i)
				index = index + rc:setUnboundedRealComps(&self.realcomps, index)
			end
			nextTrace:update(false)
			-- Accept/reject
			var targetAcceptRatio = 0.234
			if n < 5 then targetAcceptRatio = lerp(0.44, 0.234, (n-1)/4.0) end
			var acceptThresh = nextTrace.logprob - currTrace.logprob
			if nextTrace.conditionsSatisfied and tmath.log(random.random()) < acceptThresh then
				self.propsAccepted = self.propsAccepted + 1
				util.swap(@currTrace, @nextTrace)
				-- Do adaptation
				if self.adapting then
					var t = targetAcceptRatio
					self.scale = self.scale + (self.scale / (t * (1.0 - t))) * (1.0 - t)/(iter+1)
				end
			else
				-- Do adaptation
				if self.adapting then
					var t = targetAcceptRatio
					self.scale = tmath.fabs(self.scale - (self.scale / (t * (1.0 - t))) * t/(iter+1))
				end
			end
		end

		return HARMKernel

	end
end



return
{
	exports = 
	{
		HARMKernel = HARMKernel
	}	
}







