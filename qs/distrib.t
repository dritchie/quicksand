local util = terralib.require("qs.lib.util")

local S = util.require("lib.std")
local tmath = util.require("tmath")
local globals = util.require("globals")

-- TODO: Ability to swap out different uniform random number generators
local CRand = terralib.includecstring [[
#include <stdlib.h>
#include <time.h>
double random_() { return rand() / (RAND_MAX+1.0); }
void initrand_() { srand(time(NULL)); }
]]

local R = {}

R.random = CRand.random_
R.initrand = CRand.initrand_

--------------------------------------------

R.bernoulli = S.memoize(function(real)
	return {
		sample = terra(p: real) : bool
			var randval = R.random()
			return (randval < p)
		end,
		logprob = terra(val: bool, p: real) : real
			var prob: real
			if val then
				prob = p
			else
				prob = 1.0 - p
			end
			return tmath.log(prob)
		end
	}
end)

--------------------------------------------

R.uniform = S.memoize(function(real)
	return {
		sample = terra(lo: real, hi: real) : real
			var u = R.random()
			return (1.0-u)*lo + u*hi
		end,
		logprob = terra(val: real, lo: real, hi: real) : real
			if val < lo or val > hi then return [-math.huge] end
			return -tmath.log(hi - lo)
		end
	}
end)

--------------------------------------------

R.gaussian = S.memoize(function(real)
	local flt = globals.primfloat
	return {
		sample = terra(mu: real, sigma: real) : real
			var u:flt, v:flt, x:flt, y:flt, q:flt
			repeat
				u = 1.0 - R.random()
				v = 1.7156 * (R.random() - 0.5)
				x = u - 0.449871
				y = tmath.fabs(v) + 0.386595
				q = x*x + y*(0.196*y - 0.25472*x)
			until not(q >= 0.27597 and (q > 0.27846 or v*v > -4 * u * u * tmath.log(u)))
			return mu + sigma*v/u
		end,
		logprob = terra(x: real, mu: real, sigma: real) : real
			var xminusmu = x - mu
			return -.5*(1.8378770664093453 + 2*tmath.log(sigma) + xminusmu*xminusmu/(sigma*sigma))
		end
	}
end)

--------------------------------------------

local gamma_cof = global(globals.primfloat[6])
local terra init_gamma_cof()
	gamma_cof = array(76.18009172947146,
					  -86.50532032941677,
					  24.01409824083091,
					  -1.231739572450155,
					  0.1208650973866179e-2,
					  -0.5395239384953e-5)
end
init_gamma_cof()
local log_gamma = S.memoize(function(real)
	return terra(xx: real)
		var x = xx - 1.0
		var tmp = x + 5.5
		tmp = tmp - (x + 0.5)*tmath.log(tmp)
		var ser = real(1.000000000190015)
		for j=0,5 do
			x = x + 1.0
			ser = ser + gamma_cof[j] / x
		end
		return -tmp + tmath.log(2.5066282746310005*ser)
	end
end)


R.gamma = S.memoize(function(real)
	local flt = globals.primfloat
	local terra sample(shape: real, scale: real) : real
		if shape < 1.0 then return sample(1.0+shape,scale) * tmath.pow(R.random(), 1.0/shape) end
		var x:flt, v:real, u:flt
		var d = shape - 1.0/3.0
		var c = 1.0/tmath.sqrt(9.0*d)
		while true do
			repeat
				x = [R.gaussian(flt)].sample(0.0, 1.0)
				v = 1.0+c*x
			until v > 0.0
			v = v*v*v
			u = R.random()
			if (u < 1.0 - .331*x*x*x*x) or (tmath.log(u) < .5*x*x + d*(1.0 - v + tmath.log(v))) then
				return scale*d*v
			end
		end
	end
	return {
		sample = sample,
		logprob = terra(x: real, shape: real, scale: real) : real
			return (shape - 1.0)*tmath.log(x) - x/scale - [log_gamma(real)](shape) - shape*tmath.log(scale)
		end
	}
end)

--------------------------------------------

local log_beta = S.memoize(function(real)
	local lg = log_gamma(real)
	return terra(a: real, b: real)
		return lg(a) + lg(b) - lg(a+b)
	end
end)

R.beta = S.memoize(function(real)
	return {
		sample = terra(a: real, b: real) : real
			var x = [R.gamma(real)].sample(a, 1.0)
			return x / (x + [R.gamma(real)].sample(b, 1.0))
		end,
		logprob = terra(x: real, a: real, b: real) : real
			if x > 0.0 and x < 1.0 then
				return (a-1.0)*tmath.log(x) + (b-1.0)*tmath.log(1.0-x) - [log_beta(real)](a,b)
			else
				return [-math.huge]
			end
		end
	}
end)

--------------------------------------------

local g = S.memoize(function(real)
	return terra(x: real)
		if x == 0.0 then return 1.0 end
		if x == 1.0 then return 0.0 end
		var d = 1.0 - x
		return (1.0 - (x * x) + (2.0 * x * tmath.log(x))) / (d * d)
	end
end)

R.binomial = S.memoize(function(real)
	local inv2 = 1/2
	local inv3 = 1/3
	local inv6 = 1/6
	local flt = globals.primfloat
	return {
		sample = terra(p: real, n: int) : int
			var k = 0
			var N = 10
			var a:flt, b:flt
			while n > N do
				a = 1 + n/2
				b = 1 + n-a
				var x = [R.beta(flt)].sample(a, b)
				if x >= p then
					n = a - 1
					p = p / x
				else
					k = k + a
					n = b - 1
					p = (p-x) / (1.0 - x)
				end
			end
			var u:flt
			for i=0,n do
				u = R.random()
				if u < p then k = k + 1 end
			end
			return k
		end,
		logprob = terra(s: int, p: real, n: int) : real
			if s >= n then return [-math.huge] end
			var q = 1.0-p
			var S = s + inv2
			var T = n - s - inv2
			var d1 = s + inv6 - (n + inv3) * p
			var d2 = q/(s+inv2) - p/(T+inv2) + (q-inv2)/(n+1)
			d2 = d1 + 0.02*d2
			var num = 1.0 + q * [g(real)](S/(n*p)) + p * [g(real)](T/(n*q))
			var den = (n + inv6) * p * q
			var z = num / den
			var invsd = tmath.sqrt(z)
			z = d2 * invsd
			return [R.gaussian(real)].logprob(z, 0.0, 1.0) + tmath.log(invsd)
		end
	}
end)

--------------------------------------------

local terra fact(x: int)
	var t:int = 1
	while x > 1 do
		t = t * x
		x = x - 1
	end
	return t	
end

local terra lnfact(x: int)
	if x < 1 then x = 1 end
	if x < 12 then return tmath.log(fact(x)) end
	var invx = 1.0 / x
	var invx2 = invx*invx
	var invx3 = invx2*invx
	var invx5 = invx3*invx2
	var invx7 = invx5*invx2
	var ssum = ((x + 0.5) * tmath.log(x)) - x
	ssum = ssum + tmath.log(2*[math.pi]) / 2.0
	ssum = ssum + (invx / 12) - (invx / 360)
	ssum = ssum + (invx5 / 1260) - (invx7 / 1680)
	return ssum
end

R.poisson = S.memoize(function(real)
	return {
		sample = terra(mu: real) : int
			var k = 0.0
			while mu > 10 do
				var m = (7.0/8)*mu
				var x = [R.gamma(real)].sample(m, 1.0)
				if x > mu then
					return int(k + [R.binomial(real)].sample(mu/x, (m-1)))
				else
					mu = mu - x
					k = k + m
				end
			end
			var emu = tmath.exp(-mu)
			var p = 1.0
			while p > emu do
				p = p * R.random()
				k = k + 1
			end
			return int(k-1)
		end,
		logprob = terra(k: int, mu: real) : real
			return k * tmath.log(mu) - mu - lnfact(k)
		end
	}
end)

--------------------------------------------

R.multinomial_array = S.memoize(function(N)
	return S.memoize(function(real)
		return {
			sample = terra(params: real[N]) : int
				var sum = real(0.0)
				for i=0,N do sum = sum + params[i] end
				var result: int = 0
				var x = R.random() * sum
				var probAccum = real(0.0)
				repeat
					probAccum = probAccum + params[result]
					result = result + 1
				until probAccum > x or result == N
				return result - 1
			end,
			logprob = terra(val: int, params: real[N]) : real
				if val < 0 or val >= N then
					return [-math.huge]
				end
				var sum = real(0.0)
				for i=0,N do
					sum = sum + params[i]
				end
				return tmath.log(params[val]/sum)
			end
		}
	end)
end)

R.multinomial_vector = S.memoize(function(real)
	return {
		sample = terra(params: &S.Vector(real)) : int
			var sum = real(0.0)
			for i=0,params:size() do sum = sum + params(i) end
			var result: int = 0
			var x = R.random() * sum
			var probAccum = real(0.0)
			repeat
				probAccum = probAccum + params(result)
				result = result + 1
			until probAccum > x or result == params:size()
			return result - 1
		end,
		logprob = terra(val: int, params: &S.Vector(real)) : real
			if val < 0 or val >= params:size() then
				return [-math.huge]
			end
			var sum = real(0.0)
			for i=0,params:size() do
				sum = sum + params(i)
			end
			return tmath.log(params(val)/sum)
		end
	}
end)

--------------------------------------------

R.dirichlet_array = S.memoize(function(N)
	return S.memoize(function(real)
		return {
			sample = terra(params: real[N]) : real[N]
				var out : real[N]
				var ssum = real(0.0)
				for i=0,N do
					var t = [R.gamma(real)].sample(params[i], 1.0)
					out[i] = t
					ssum = ssum + t
				end
				for i=0,N do
					out[i] = out[i]/ssum
				end
				return out
			end,
			logprob = terra(theta: real[N], params: real[N]) : real
				var sum = real(0.0)
				for i=0,N do sum = sum + params[i] end
				var logp = [log_gamma(real)](sum)
				for i=0,N do
					var a = params[i]
					logp = logp + (a - 1.0)*tmath.log(theta[i])
					logp = logp - [log_gamma(real)](a)
				end
				return logp
			end
		}
	end)
end)

R.dirichlet_vector = S.memoize(function(real)
	return {
		-- NOTE: It is up to the caller to manage the memory of the
		--    returned vector.
		sample = terra(params: &S.Vector(real)) : S.Vector(real)
			var out : S.Vector(real)
			out:init(params:size())
			for i=0,params:size() do out:insert() end
			var ssum = real(0.0)
			for i=0,params:size() do
				var t = [R.gamma(real)].sample(params(i), 1.0)
				out(i) = t
				ssum = ssum + t
			end
			for i=0,params:size() do
				out(i) = out(i)/ssum
			end
			return out
		end,
		logprob = terra(theta: &S.Vector(real), params: &S.Vector(real)) : real
			var sum = real(0.0)
			for i=0,params:size() do sum = sum + params(i) end
			var logp = [log_gamma(real)](sum)
			for i=0,params:size() do
				var a = params(i)
				logp = logp + (a - 1.0)*tmath.log(theta(i))
				logp = logp - [log_gamma(real)](a)
			end
			return logp
		end
	}
end)

--------------------------------------------

return R



