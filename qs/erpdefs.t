local util = terralib.require("qs.lib.util")

local S = util.require("lib.std")
local erp = util.require("erp")
local distrib = util.require("distrib")
local tmath = util.require("tmath")
local globals = util.require("globals")



-- Insert lower and/or upper bounds into an opts struct.
-- Bounds that were already specified are left unchanged.
local function insertBounds(opts, lo, hi)
	local LO = erp.Options.LowerBound
	local HI = erp.Options.UpperBound
	if not opts then
		if lo and hi then return `{[LO]=lo, [HI]=hi} end
		if lo then return `{[LO]=lo} end
		if hi then return `{[HI]=hi} end
	end
	local T = opts:gettype()
	local thaslo = false
	local thashi = false
	local struct NewT {}
	for _,e in ipairs(T.entries) do
		NewT.entries:insert({field=e.field, type=e.type})
		if e.field == LO then thaslo = true end
		if e.field == HI then thashi = true end
	end
	if lo and not thaslo then
		NewT.entries:insert({field=LO, type=globals.real})
	end
	if hi and not thashi then
		NewT.entries:insert({field=HI, type=globals.real})
	end
	return quote
		var newopts = NewT(opts)
		escape
			if lo and not thaslo then
				emit quote newopts.[LO] = lo end
			end
			if hi and not thashi then
				emit quote newopts.[HI] = hi end
			end
		end
	in
		newopts
	end
end




local ERPs = {}

--------------------------------------------

ERPs.flip = erp.makeRandomChoice(
	distrib.bernoulli,
	S.memoize(function(real)
		return terra(currval: bool, p: real) : {bool, real, real}
			if currval then
				return false, real(0.0), real(0.0)
			else
				return true, real(0.0), real(0.0)
			end
		end
	end)
)

--------------------------------------------

local uniform = erp.makeRandomChoice(
	distrib.uniform
)
-- Uniforms are lower and upper bounded
ERPs.uniform = macro(function(lo, hi, opts)
	return `uniform(lo, hi, [insertBounds(opts, lo, hi)])
end)
ERPs.uniform.observe = macro(function(val, lo, hi, opts)
	return `uniform.observe(val, lo, hi, [insertBounds(opts, lo, hi)])
end)

--------------------------------------------

ERPs.gaussian = erp.makeRandomChoice(
	distrib.gaussian,
	-- Drift kernel
	S.memoize(function(real)
		return terra(currval: real, mu: real, sigma: real)
			var newval = [distrib.gaussian(real)].sample(currval, sigma)
			var fwdlp = [distrib.gaussian(real)].logprob(newval, currval, sigma)
			var rvslp = [distrib.gaussian(real)].logprob(currval, newval, sigma)
			return newval, fwdlp, rvslp
		end
	end)
)

--------------------------------------------

local gamma = erp.makeRandomChoice(
	distrib.gamma
)
-- Gammas are non-negative
ERPs.gamma = macro(function(shape, scale, opts)
	return `gamma(shape, scale, [insertBounds(opts, `0.0)])
end)
ERPs.gamma.observe = macro(function(val, shape, scale, opts)
	return `gamma.observe(val, shape, scale, [insertBounds(opts, `0.0)])
end)

-- Often more convenient to specify a gamma in terms of mean and variance
ERPs.gammamv = macro(function(m, v, opts)
	opts = opts or `{}
	return quote
		var shape = m*m / v
		var scale = v / m
	in
		ERPs.gamma(shape, scale, opts)
	end
end)
ERPs.gammamv.observe = macro(function(val, m, v, opts)
	opts = opts or `{}
	return quote
		var shape = m*m / v
		var scale = v / m
	in
		ERPs.gamma.observe(val, shape, scale, opts)
	end
end)

--------------------------------------------

local beta = erp.makeRandomChoice(
	distrib.beta
)
-- Betas are bounded to the range (0,1)
ERPs.beta = macro(function(a, b, opts)
	return `beta(a, b, [insertBounds(opts, `0.0, `1.0)])
end)
ERPs.beta.observe = macro(function(val, a, b, opts)
	return `beta.observe(val, a, b, [insertBounds(ops, `0.0, `1.0)])
end)

-- Often more convenient to specify a beta in terms of mean and variance
ERPs.betamv = macro(function(m, v, opts)
	opts = opts or `{}
	return quote
		var tmp = m*m - m + v
		var a = - m * tmp / v
		var b = (m - 1) * tmp / v
	in
		ERPs.beta(a, b, opts)
	end
end)
ERPs.betamv.observe = macro(function(val, m, v, opts)
	opts = opts or `{}
	return quote
		var tmp = m*m - m + v
		var a = - m * tmp / v
		var b = (m - 1) * tmp / v
	in
		ERPs.beta.observe(val, a, b, opts)
	end
end)

--------------------------------------------

ERPs.binomial = erp.makeRandomChoice(
	distrib.binomial
)

--------------------------------------------

ERPs.poisson = erp.makeRandomChoice(
	distrib.poisson
)

--------------------------------------------

-- We expose one multinomial random choice that handles
--    both array and Vector parameters.

local multinomial_array = S.memoize(function(N)
	return erp.makeRandomChoice(distrib.multinomial_array(N))
end)
local multinomial_vector = erp.makeRandomChoice(
	distrib.multinomial_vector
)

ERPs.multinomial = macro(function(params, opts)
	opts = opts or `{}
	local T = params:gettype()
	if T:isarray() and T.type == globals.real then
		return `[multinomial_array(T.N)](params, opts)
	elseif T == &S.Vector(globals.real) then
		return `multinomial_vector(params, opts)
	else
		error("multinomial params must be an array or Vector of reals")
	end
end)
ERPs.multinomial.observe = macro(function(val, params, opts)
	opts = opts or `{}
	local T = params:gettype()
	if T:isarray() and T.type == globals.real then
		return `[multinomial_array(T.N)].observe(val, params, opts)
	elseif T == &S.Vector(globals.real) then
		return `multinomial_vector(val, params, opts)
	else
		error("multinomial params must be an array or Vector of reals")
	end
end)

--------------------------------------------

-- dirichlet is set up just like multinomial

local dirichlet_array = S.memoize(function(N)
	return erp.makeRandomChoice(distrib.dirichlet_array(N))
end)
local dirichlet_vector = erp.makeRandomChoice(
	distrib.dirichlet_vector
)

ERPs.dirichlet = macro(function(params, opts)
	opts = opts or `{}
	local T = params:gettype()
	if T:isarray() and T.type == globals.real then
		return `[dirichlet_array(T.N)](params, opts)
	elseif T == &S.Vector(globals.real) then
		return `dirichlet_vector(params, opts)
	else
		error("dirichlet params must be an array or Vector of reals")
	end
end)
ERPs.dirichlet.observe = macro(function(val, params, opts)
	opts = opts or `{}
	local T = params:gettype()
	if T:isarray() and T.type == globals.real then
		return `[dirichlet_array(T.N)].observe(val, params, opts)
	elseif T == &S.Vector(globals.real) then
		return `dirichlet_vector(val, params, opts)
	else
		error("dirichlet params must be an array or Vector of reals")
	end
end)

--------------------------------------------

-- General scalar random choice
-- Chooses a type of distribution based on what options it receives.
-- (Makes maximum entroy decisions whenever possible)
local function genScalar(opts, obsval)

	local function g(f)
		return macro(function(...)
			local args = {...}
			if obsval then return `f.observe(obsval, [args]) end
			return `f([args])
		end)
	end
	local LO = erp.Options.LowerBound
	local HI = erp.Options.UpperBound

	-- Possible options are: mean, vari, lo, hi
	local haslo = erp.structHasMember(opts, LO)
	local hashi = erp.structHasMember(opts, HI)
	local hasmean = erp.structHasMember(opts, "mean")
	local hasvari = erp.structHasMember(opts, "vari")

	opts = opts or `{}

	-- Two-sided bounded options
	if haslo and hashi then
		local rng = `opts.[HI] - opts.[LO]
		if hasmean and hasvari then
			return `opts.[LO] + rng * [g(ERPs.betamv)]((opts.mean-opts.[LO])/rng, opts.vari/rng, opts)
		end
		if hasmean then
			-- variance here is that of a beta(1,1)
			return `opts.[LO] + rng * [g(ERPs.betamv)]((opts.mean-opts.[LO])/rng, 1.0/12.0, opts)
		end
		if hasvari then
			return `opts.[LO] + rng * [g(ERPs.betamv)](0.5, opts.vari/rng, opts)
		end
		return `[g(ERPs.uniform)](opts.[LO], opts.[HI], opts)
	-- Lower bounded options
	elseif haslo then
		if hasmean and hasvari then
			return `opts.[LO] + [g(ERPs.gammamv)](opts.mean - opts.[LO], opts.vari, opts)
		end
		if hasmean then
			return `opts.[LO] + [g(ERPs.gammamv)](opts.mean - opts.[LO], 1.0, opts)
		end
		if hasvari then
			return `opts.[LO] + [g(ERPs.gammamv)](1.0, opts.vari, opts)
		end
		return `opts.[LO] + [g(ERPs.gammamv)](1.0, 1.0, opts)
	-- Upper bounded options
	elseif hashi then
		if hasmean and hasvari then
			return `opts.[HI] - [g(ERPs.gammamv)](opts.[HI] - opts.mean, opts.vari, opts)
		end
		if hasmean then
			return `opts.[HI] - [g(ERPs.gammamv)](opts.[HI] - opts.mean, 1.0, opts)
		end
		if hasvari then
			return `opts.[HI] - [g(ERPs.gammamv)](1.0, opts.vari, opts)
		end
		return `opts.[HI] - [g(ERPs.gammamv)](1.0, 1.0, opts)
	-- Unbounded options
	else
		if hasmean and hasvari then
			return `[g(ERPs.gaussian)](opts.mean, tmath.sqrt(opts.vari), opts)
		end
		if hasmean then
			return `[g(ERPs.gaussian)](opts.mean, 1.0, opts)
		end
		if hasvari then
			return `[g(ERPs.gaussian)](0.0, tmath.sqrt(opts.vari), opts)
		end
		return `[g(ERPs.gaussian)](0.0, 1.0, opts)
	end
end

ERPs.scalar = macro(function(opts)
	return genScalar(opts, false)
end)
ERPs.scalar.observe = macro(function(val, opts)
	return genScalar(opts, val)
end)

--------------------------------------------



-- TODO: Wishlist
--    * Extensible bounding behavior, especially simplex bounds for dirichlet
--      (right now it's only safe to use bounds/HMC with scalar variables)
--    * Circular/spherical distributions (e.g. von Mises, von Mishes-Fisher)
--    * Other distributions? (matrices, etc.?)



return 
{
	exports = ERPs
}







