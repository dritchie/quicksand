local util = terralib.require("qs.lib.util")

local S = util.require("lib.std")
local erp = util.require("erp")
local distrib = util.require("distrib")
local tmath = util.require("lib.tmath")
local qs = util.require("globals")



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

ERPs.uniform = erp.makeRandomChoice(
	distrib.uniform,
	nil,
	erp.Bounds.LowerUpper(function(real)
		return terra(lo: real, hi: real)
			return lo, hi
		end
	end)
)

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

ERPs.gamma = erp.makeRandomChoice(
	distrib.gamma,
	nil,
	erp.Bounds.Lower(function(real)
		return terra(shape: real, scale: real)
			return 0.0
		end
	end)
)

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
ERPs.gammamv.observe = macro(function(val, m, v)
	return quote
		var shape = m*m / v
		var scale = v / m
	in
		ERPs.gamma.observe(val, shape, scale)
	end
end)

--------------------------------------------

ERPs.beta = erp.makeRandomChoice(
	distrib.beta,
	nil,
	erp.Bounds.LowerUpper(function(real)
		return terra(a: real, b: real)
			return 0.0, 1.0
		end
	end)
)

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
ERPs.betamv.observe = macro(function(val, m, v)
	return quote
		var tmp = m*m - m + v
		var a = - m * tmp / v
		var b = (m - 1) * tmp / v
	in
		ERPs.beta.observe(val, a, b)
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
	if T:isarray() and T.type == qs.real then
		return `[multinomial_array(T.N)](params, opts)
	elseif T == &S.Vector(qs.real) then
		return `multinomial_vector(params, opts)
	else
		error("multinomial params must be an array or &Vector of reals")
	end
end)
ERPs.multinomial.observe = macro(function(val, params)
	local T = params:gettype()
	if T:isarray() and T.type == qs.real then
		return `[multinomial_array(T.N)].observe(val, params)
	elseif T == &S.Vector(qs.real) then
		return `multinomial_vector.observe(val, params)
	else
		error("multinomial params must be an array or &Vector of reals")
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
	if T:isarray() and T.type == qs.real then
		return `[dirichlet_array(T.N)](params, opts)
	elseif T == &S.Vector(qs.real) then
		return `dirichlet_vector(params, opts)
	else
		error("dirichlet params must be an array or &Vector of reals")
	end
end)
ERPs.dirichlet.observe = macro(function(val, params)
	local T = params:gettype()
	if T:isarray() and T.type == qs.real then
		return `[dirichlet_array(T.N)].observe(val, params)
	elseif T == &S.Vector(qs.real) then
		return `dirichlet_vector.observe(val, params)
	else
		error("dirichlet params must be an array or &Vector of reals")
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

	-- Possible options are: mean, vari, lo, hi
	local haslo = erp.structHasMember(opts, "lo")
	local hashi = erp.structHasMember(opts, "hi")
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
--    * Circular/spherical distributions (e.g. von Mises, von Mishes-Fisher)
--    * Other distributions? (matrices, etc.?)



return 
{
	exports = ERPs
}







