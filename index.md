---
layout: post
title: Quicksand
---

# Getting Started with Quicksand

Quicksand is a library for [probabilistic programming](http://web.stanford.edu/~ngoodman/papers/POPL2013-abstract.pdf) in the Terra programming language. Since Terra is a low-level language, Quicksand programs are written a level of abstraction comparable to that of C. Users can write these programs directly, or potentially use Quicksand as a compiler target for higher-level/domain-specific languages.


## Installation

You'll first need to download and build [Terra](http://terralang.org).

Then just add the following to your `.profile`, `.bashrc`, or equivalent file:

	export QS=[path to quicksand root]
	export LUA_PATH="$LUA_PATH;$QS/?.t/$QS/?/init.t"

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


## Random Choices


## Likelihoods and Conditioning

### qs.condition(bool)

### qs.conditionfunc(terrafn)

### qs.factor(real)

### qs.factorfunc(terrafn)

### [randomChoice].observe(value, ...)


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

