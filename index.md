---
layout: post
title: Quicksand
---

# Getting Started with Quicksand

Quicksand is a library for [probabilistic programming](http://web.stanford.edu/~ngoodman/papers/POPL2013-abstract.pdf) in the Terra programming language. Since Terra is a low-level language, Quicksand programs are written a level of abstraction comparable to that of C. Users can write these programs directly, or potentially use Quicksand as a compiler target for higher-level/domain-specific languages.

* auto-gen TOC:
{:toc}

## Installation

You'll first need to download and build [Terra](http://terralang.org).

Then just add the following to your `.profile`, `.bashrc`, or equivalent file:

	export QS=[path to quicksand root]
	export LUA_PATH="$LUA_PATH;$QS/?.t;$QS/?/init.t"

Quicksand has been tested on OSX and should also work on Linux. On Windows, your mileage may vary.


## Simple Quicksand Programs

Let's take a look at a simple Quicksand program--a probabilistic "Hello, World," if you will:

	local qs = terralib.require("qs")

	local p1 = qs.program(function()
		return terra()
			var a = qs.flip(0.5)
			var b = qs.flip(0.5)
			qs.condition(a or b)
			return (a and b)
		end
	end)

The probabilistic program `p1` draws two random values via unbiased coin flips (the 0.5 means "50% chance of coming up true") and returns the logical AND of those two values, subject to the constraint that at least one of them is true. 

Here's a slightly more complex (and useful) example: estimating parameters of a Gaussian mixture model from data:

	local qs = terralib.require("qs")
	local S = terralib.require("qs.lib.std")

	local p2 = qs.program(function()
		
		local data = global(S.Vector(qs.real))

		-- Initialize data vector...

		return terra()
			var mixparams = qs.dirichlet(array(1.0, 1.0, 1.0))
			var means = array(qs.gaussian(0.0, 1.0),
							  qs.gaussian(0.0, 1.0),
							  qs.gaussian(0.0, 1.0))
			for d in data do
				var which = qs.multinomial(mixparams)
				qs.gaussian.observe(d, means[which], 1.0)
			end
			return mixparams, means
		end

	end)

Here, the program `p2` draws mixture parameters and mixture component means from Dirichlet and Gaussian priors, respectively. For each data point, it then selects which mixture component the data point was drawn from (a latent variable), and then accounts for the probability of drawing that data point from that mixture component (here, all components have a constant standard deviation of 1).

Probabilistic programs can naturally represent distributions with a variable number of random variables. Here, we'll show how to use this ability to perform model selection by inferring the number of components in our mixture model:

	local qs = terralib.require("qs")
	local S = terralib.require("qs.lib.std")

	local p3 = qs.program(function()
		
		local data = global(S.Vector(qs.real))

		-- Initialize data vector...

		return terra()
			var nums = array(2, 3, 4, 5)
			var n = nums[qs.multinomial(0.25, 0.25, 0.25, 0.25)]
			var mixprior  = [S.Vector(qs.real)].salloc():init()
			var means = [S.Vector(qs.real)].salloc():init()
			for i=0,n do
				mixprior:insert(1.0)
				means:insert(qs.gaussian(0.0, 1.0))
			end
			var mixparams = qs.dirichlet(mixprior)
			for d in data do
				var which = qs.multinomial(mixparams)
				qs.gaussian.observe(d, means[which], 1.0)
			end
			return n
		end

	end)

In the rest of this document, we'll describe the components that go into building these programs, as well as the procedures available for performing inference on them.


## Real Numbers

Within a probabilistic program, any real (i.e. floating point) number used should have the type `qs.real`. This defaults to `double`, but can be changed by modifying the value of `qs.primfloat` in `qs/globals.t`. Inference methods can also change the value of `qs.real` (e.g. Hamiltonian Monte Carlo).


## Random Choices

Random choices are what make probabilistic programs probabilistic. A program makes some number of random choices during its execution, and inference methods then manipulate the values of those random choices to explore the space of possible program executions.

### Primitive Random Choice Functions

Quicksand has a number of built-in random choice functions:

`qs.flip(p)`  
Flip a coin with weight `p` (i.e. a Bernoulli distribuion).

`qs.uniform(lo, hi)`  
A number uniformly distributed between `lo` and `hi`.

`qs.gaussian(m, sd)`  
A number normally distributed with mean `m` and standard deviation `sd`.

`qs.gamma(k, theta)`  
Draw from a gamma distribution with shape parameter `k` and scale parameter `theta`.

`qs.gammamv(m, v)`  
Draw from a gamma distribution with mean `m` and variance `v`. This is just syntax sugar on top of `qs.gammma`.

`qs.beta(a, b)`  
Draw from a beta distribution with shape parameters `a` and `b`.

`qs.betamv(m, v)`  
Draw from a beta distribution with mean `m` and variance `v`. This is just syntax sugar on top of `qs.beta`.

`qs.binomial(p, n)`  
Draw from a binomial distribution with probability `p` and number of trials `n`.

`qs.poisson(lambda)`  
Draw from a poisson distribution with average rate `lambda`.

`qs.multinomial(params)`  
Draw from a multinomial distribution. `params` may be either an array or a vector of real numbers.

`qs.dirichlet(params)`  
Draw from a dirichlet distribution. `params` may be either an array or a vector of real numbers.

### Options

In addition to their native parameters, every random choice can also take a set of options, specified as a struct literal:

	qs.gaussian(0.0, 1.0, {init=0.0, struc=false})

`init`  
Specifies the initial value of the random choice for MCMC inference.

`struc`  
Declares whether this random choice can affect the *structure* of the program execution trace--more specifically, whether the value of this random choice can determine the existence of other random choices. This must be a boolean compile-time constant (`true`, `false`, or a Lua boolean variable), and it defaults to `true` for all choices unless otherwise specified. Some inference methods depend on being able to separate structural vs. non-structural random choices. In principle, whether a choice is structural can be determined automatically through program analysis. To keep the system as simple as possible, though, Quicksand requires manual specification. It is usually not difficult to identify when a choice is non-structural (most random choices are, except for some latent variables in hierarchical or recursive models).

### Creating New Random Choice Functions

You can define your own primitive random choice functions using `makeRandomChoice` in `qs/erp.t`. See `qs/erpdefs.t` for usage examples. The interface to random choice creation is subject to change, so it is not documented in any further detail here.


## Likelihoods and Conditioning

A probabilistic program is only interesting if we introduce some condition or constraint its execution--otherwise, we could just run it forward to obtain samples from the distribution it defines.

### [randomChoice].observe(value, ...)

Adjusts the probability of the current program execution according to the probability of observing `value` drawn from `[randomChoice]` with parameters `...`. The simple example programs above demonstrated

### qs.condition(bool)

### qs.conditionfunc(terrafn)

### qs.factor(real)

### qs.factorfunc(terrafn)


## Function and Loops

### qs.func(terrafn)

### qs.method(terramethod)

### qs.range(lo, hi)


## Programs and Modules

### qs.program(luafn)

### qs.module(luafn)


## Inference

### qs.infer(prog, query, method)


## Methods

### qs.ForwardSample(nsamps)

### qs.WeightedRejectionSample(nsamps)

### qs.MCMC(kernel, params)


## Queries

### qs.Samples

### qs.Expectation(alsoVariance)

### qs.MAP

### qs.Autocorrelation(mean, variance)

### qs.Histogram

