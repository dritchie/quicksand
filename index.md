---
layout: post
title: Quicksand
---

<h1>Getting Started with Quicksand</h1>

Quicksand is a library for [probabilistic programming](http://web.stanford.edu/~ngoodman/papers/POPL2013-abstract.pdf) in the Terra programming language. Since Terra is a low-level language, Quicksand programs have a level of abstraction comparable to that of C. Users can write these programs directly, or potentially use Quicksand as a compiler target for higher-level/domain-specific languages.

<h1>Table of Contents</h1>
* auto-gen TOC:
{:toc}

# Installation

You'll first need to download and build [Terra](http://terralang.org).

Then just add the following to your `.profile`, `.bashrc`, or equivalent file:

	export QS=[path to quicksand root]
	export LUA_PATH="$LUA_PATH;$QS/?.t;$QS/?/init.t"

To verify that everything's working correctly, you can run the test suite and check that all tests pass:

	terra tests/testsuite.t

Since Quicksand is by its nature stochastic, a test can very occasionally fail here or there ('transdimensional expectation' is the most frequent culprit). This is nothing to worry about.

Quicksand has been tested on OSX and should also work on Linux. On Windows, your mileage may vary.


# Simple Quicksand Programs

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


# Real Numbers

Within a probabilistic program, any real (i.e. floating point) number used should have the type `qs.real`. This defaults to `double`, but can be changed by modifying the value of `qs.float` in `qs/globals.t`. Inference methods can also change the value of `qs.real` (e.g. Hamiltonian Monte Carlo).


# Random Choices

Random choices are what make probabilistic programs probabilistic. A program makes some number of random choices during its execution, and inference methods then manipulate the values of those random choices to explore the space of possible program executions.

## Primitive Random Choice Functions

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

## Options

In addition to their native parameters, every random choice can also take a set of options, specified as a struct literal:

	qs.gaussian(0.0, 1.0, {init=0.0, struc=false})

`init`  
Specifies the initial value of the random choice for MCMC inference.

`struc`  
Declares whether this random choice can affect the *structure* of the program execution trace--more specifically, whether the value of this random choice can determine the existence of other random choices. This must be a boolean compile-time constant (`true`, `false`, or a Lua boolean variable), and it defaults to `true` for all choices unless otherwise specified. Some inference methods depend on being able to separate structural vs. non-structural random choices. In principle, whether a choice is structural can be determined automatically through program analysis. To keep the system as simple as possible, though, Quicksand requires manual specification. It is usually not difficult to identify when a choice is non-structural (most random choices are, except for some latent variables in hierarchical or recursive models).

## Creating New Random Choice Functions

You can define your own primitive random choice functions using `makeRandomChoice` in `qs/erp.t`. See `qs/erpdefs.t` for usage examples. The interface to random choice creation is subject to change, so it is not documented in any further detail here.


# Likelihoods and Conditioning

A probabilistic program is only interesting if we introduce some condition or constraint on its execution--otherwise, we could just run it forward to obtain samples from the distribution it defines.

### observe

Every random choice comes equipped with a method to observe (or condition on) a value, instead of drawing one randomly. For example:

	qs.gaussian.observe(0.2, 0.0, 1.0)

This adjusts the probability of the current program execution according to the probability of observing the value `0.2` draw from a standard normal distribution.

### qs.condition

Imposes a hard constraint on a program's execution:

	var x = qs.beta(2.0, 2.0)
	qs.condition(x > 0.5)

This will cause all program executions with `x <= 0.5` to be rejected. Imposing many or difficult-to-satisfy constraints in this way can cause inference to become inefficient, however.

### qs.conditionfunc

Automatically applies the return value of a function as a condition:

	local p = qs.program(function()
		local cf = qs.conditionfunc(terra(x: qs.real)
			return x > 0.5
		end)
		return terra()
			var x = qs.beta(2.0, 2.0)
			cf(x)
		end
	end)

While overkill for the example above, this can be useful for encapsulating conditions that require extensive computation to compute. Additionally, some inference methods can sometimes skip executing these functions when it is not necessary for their operation, which can be a computation savings.

### qs.factor

Directly adds to the log probability of the current program execution:

	var x = qs.flip(0.5)
	var y = qs.flip(0.5)
	if x == y then qs.factor(100.0) end
	if x ~= y then qs.factor(-100.0) end

Useful for implementing undirected models such as Ising models and Markov Random Fields.

### qs.factorfunc

Is to `qs.factor` what `qs.conditionfunc` is to `qs.condition`. The same addendum about potential computation savings also applies here.


# Functions and Loops

Function calls and loops can affect the control flow structure of a program, so Quicksand provides some constructs to help inform inference methods about this structure.

### qs.func

All Terra functions that make random choices (or whose callees make random choices) should be wrapped with `qs.func`:

	local f = qs.func(terra(m: qs.real, sd: qs.real)
		return qs.gaussian(m, sd)
	end)

Slightly different syntax is necessary for recursive functions:

	local g = qs.func()
	g:define(terra(p: qs.real)
		if qs.flip(p) then
			return 0.0
		else
			return 1.0 + g(p)
		end
	end)

Note that it is entirely possible to use unwrapped Terra functions in your probabilistic programs: Quicksand will not warn you about this (and in fact has no mechanism to do so). Inference will always be correct regardless, but using `qs.func` will make it more efficient for programs that exhibit structure change (and especially for recursive programs).

### qs.method

A variant of `qs.func` for wrapping struct methods:

	local struct Blob { val: qs.real }
	Blob.methods.sampleVal = qs.method(terra(self: &Blob)
		self.val = qs.uniform(0.0, 1.0) end
	end)

### qs.range

Provides an iterable range of integers:

	var sum = 0
	for i in qs.range(0, 10) do
		sum = sum + int(qs.flip(0.5))
	end

Other looping constructs, such as native for loops, while loops, or other iterator-based loops are also perfectly fine to use with Quicksand. However, `qs.range` tracks information about loop nesting, so using it allows more efficient inference in e.g. programs that use nested loops where the number of loop iterations are derived from random choices.


# Programs and Modules

Quicksand has constructs for separating probabilistic code (i.e. code that makes random choices) from vanilla Terra code.

### qs.program

Defines a probabilistic program. This is the object that inference methods operate on. It takes one argument, a Lua function, and that Lua function should return a no-argument Terra function--the 'main' function of the program, essentially:

	local p = qs.program(function()

		-- Define helper functions, types, etc.

		return terra()

			-- Make random choices, invoke other functions, etc.

			return [something]

		end
	end)

Any code that makes random choices (functions, struct methods) **must** be defined inside of a `qs.program`. Code within a probabilistic program goes through a special compilation process. Attempting to define probabilistic code outside of probabilistic program results in undefined behavior (most likely manifesting as inscrutable error messages).

### qs.module

For large projects, it can be useful to develop probabilistic programs modularly, instead of putting all of your code in one giant, monolithic `qs.program`. `qs.module` allows you to do this:

	local mod = qs.module(function() 
		return
		{
			dirichletMultinomial3 = qs.func(terra()
				return qs.multinomial(qs.dirichlet(array(1.0, 1.0, 1.0)))
			end)
		}
	end)

The argument to `qs.module` is a Lua function that can return anything (but typically a Lua table of functions and types). To use this module in a probabilistic program, it has to be "opened" with the `:open` method:

	local p = qs.program(function()
		local m = mod:open()
		return terra()
			var items = array(10, 22, 30)
			var choices : int[10]
			for i=0,10 do
				choices[i] = m.dirichletMultinomial3()
			end
			qs.condition(choices[0] == 22)
			return choices 
		end
	end)

Additionally, you might have some code defined inside a probabilistic program that you'd also like to use outside of it. For instance, the return value of your program is a struct with some probabilistic methods, and you'd like to do some processing on it. `qs.module` facilitates this use case, too, with the `:openAs` method:

	local mod = qs.module(function()

		local struct Vec { data: qs.real[2] }

		Vec.methods.sampleRand = qs.method(terra(self: &Vec)
			self.data[0] = qs.gaussian(0.0, 1.0, {struc=false})
			self.data[1] = qs.gaussian(0.0, 1.0, {struc=false})
		end)

		terra Vec:normSq()
			return self.data[0]*self.data[0] +
				   self.data[1]*self.data[1]
		end 

		return { Vec = Vec }
	end)

	local p = qs.program(function()
		local m = mod:open()
		return terra()
			var vecs : m.Vec[10]
			for i=0,10 do
				vecs[i]:sampleRand()
				qs.condition(vecs[i]:normSq() > 0.5)
			end
			return vecs
		end
	end)

	-- Function for processing the return value
	local m = mod:openAs(p)
	local terra totalNorm(vecs: m.Vec[10])
		var sum = vecs[0]:normSq()
		for i=1,10 do sum = sum + vecs[i]:normSq() end
		return sum
	end

`:openAs` is necessary because probabilistic code compiles differently depending on the program that is using it. In fact, it is an error to use `:open` outside of a `qs.program`.


# Inference

Finally, we get to the real heart of Quicksand--performing inference on programs.

### qs.infer

This is the entrypoint for all inference in Quicksand. It takes three arguments: a probabilistic program, an inference query to perform, and a method to use to answer that query. For example:

	local p = qs.program(function()
		return terra()
			return qs.gaussian(0.0, 0.2, {struc=false})
		end
	end)

	-- qs.infer(program, query, method)
	local samples = qs.infer(p, qs.Samples, qs.ForwardSample(1000))()

`qs.infer` returns a no-argument Terra function, which, when invoked, performs the requested inference (hence the extra set of parens at the end of the last line above).

Next, we'll cover the interface to method and query types, including those that are built-in to Quicksand.


## Methods

Inference in Quicksand is based on sampling, so inference *methods* are procedures that describe how to obtain samples from a probablistic program. Specifically, a method is a Lua function that takes a `qs.program` and return a Terra function that fills in a vector of samples (see `qs/infer.t` for precise details). Quicksand comes with several built-in methods:

### qs.ForwardSample

Draw samples by simply running the program forward repeatedly. This ignores any likelihood or conditioning statements. Useful for testing and debugging, but not a whole lot else. Takes one argument: the number of samples to draw (e.g. `qs.ForwardSample(1000)`).

### qs.WeightedRejectionSample

Draw samples by forward sampling, but reject any samples that do not satisfy conditioning statements and weight all samples by any likelihood statements. This is the simplest method that respects the full semantics of a probabilistic program, but it's often not very efficient. Useful mostly as a baseline for comparison / sanity checking. Also takes one argument: the number of samples to draw (e.g. `qs.WeightedRejectionSample(1000)`).

### qs.MCMC

Draw samples by Markov Chain Monte Carlo. This is the go-to method for inference in most cases. It takes two arguments: an MCMC transition kernel, and a table of other parameters:

	local p = qs.program(function()
		return terra()
			var sites : bool[100]
			for i=0,100 do sites[i] = qs.flip(0.5, {struc=false}) end
			for i=0,99 do 
				if sites[i] == sites[i+1] then
					qs.factor(100.0)
				else
					qs.factor(-100.0)
				end
			end	
			return sites
		end
	end)

	-- qs.MCMC(kernel, params)
	local samples = qs.infer(p, qs.Samples,
							 qs.MCMC(qs.TraceMHKernel(),
							 		 {numsamps=1000, lag=1, verbose=true}))

`numsamps`  
How many samples to draw. Defaults to 1000.

`lag`  
How many iterations of MCMC should be performed between recorded samples. Increasing this value decreases autocorrelation between samples but increases computation time. Defaults to 1.

`verbose`  
Whether Quicksand should write out MCMC analytics. Can be `true`, `false`, or a C-style `stdio.h` file handle (i.e. a `FILE*`). Defaults to `true`, in which case Quicksand writes to `stdout`.

The actual legwork of MCMC is done by *transition kernels*, which describe how to take a program execution trace and randomly modify some of its random choices such that the stationary distribution defined by doing this over and over again is the same as the distribution defined by the program. You can define your own transition kernels (see `qs/mcmc.t` for the inferface and examples), but Quicksand provides several already that cover many use cases:

`qs.TraceMHKernel({doStruct, doNonstruct})`  
The workhorse of probabilistic programming inference. This kernel implements the MCMC trace sampler described in Algorithm 2 of [this paper](http://www.mit.edu/~ast/papers/lightweight-mcmc-aistats2011.pdf). It will work on any probabilistic program, though it is not necessarily the most efficient kernel. This is often a good kernel to start with. The optional parameters `doStruct` and `doNonstruct` specify whether the kernel should operate on structural or non-structural random choices. They both default to `true` (i.e. operating on all random choices)

`qs.LARJKernel({annealKernel, intervals, stepsPerInterval})`  
Implements the [Locally-Annealed Reversible-Jump MCMC](http://web.stanford.edu/~ngoodman/papers/owl.pdf) algorithm. This kernel is particularly useful for structure-changing programs where structure changes are persistently rejected using `qs.TraceMHKernel`. The `annealKernel` option should specify a kernel that operates on only nonstructural choices--this is the kernel applied during the annealing transition phase of the algorithm (defaults to `qs.TraceMHKernel({doStruct=false})`). The `intervals` option specifies how many discrete interpolation intervals the algorithm should use in interpolating the old structure to the new structure (defaults to `0`, i.e. vanilla reversible-jump MCMC), and `stepsPerInterval` specifies the number of annealing kernel applications to use for at each interval (defaults to `1`).

`qs.HARMKernel({scale, doScaleAdapt})`  
Implements the [Hit And Run Metropolis](http://www.bayesian-inference.com/mcmcharm) algorithm. This kernel is only applicable to non-structural random choices with continuous values. It is very computationally efficient, updating all non-structural choices while requiring only one program evaluation. The option `scale` determines the largest possible jump the kernel will take (defaults to `1.0`), and the 'doScaleAdapt' flag determines whether the kernel automatically adapts its scale to achieve a target optimal acceptance ratio (defaults to `true`).

`qs.DriftKernel({scale, doScaleAdapt, lexicalScaleSharing})`  
Implements the [Random Walk Metropolis](http://www.bayesian-inference.com/mcmcrwm) algorithm using gaussian drift proposals with a diagonal covariance matrix. This kernel is only applicable to non-structural random choices with continuous values. Like `qs.HARMKernel`, this kernel also updates all non-structural chioces with only one program evaluation. The `scale` option specifies the standard deviation for gaussian proposals (defaults to `1.0`). The `doScaleAdapt` flag, when `true`, causes the kernel to automatically adapt individual scales for every random choice (defaults to `true`). If the `lexicalScaleSharing` flag is `true`, then all random choices originating from the same source code location in the program will share one adapted proposal scale (defaults to `false`). In some models that iteratively or recursively make many of the same "type" of random choice, this statistical sharing can help the kernel find better proposals more quickly (especially for new random choices introduced by structure change).

`qs.HMCKernel({stepSize, numSteps, doStepSizeAdapt})`  
Implements the [Hamiltonian Monte Carlo](http://arxiv.org/pdf/1206.1901.pdf) algorithm. This kernel is only applicable to non-structural random choices with continuous values. It is particularly useful for cases where multiple random choices are tightly constrained via factor or observe statements. The `stepSize` option specifies the step size for the leapfrog integrator used to generate proposals (defaults to `1.0`). The `numSteps` option specifies how many leapfrog steps should be used per proposal (defaults to `1`, i.e. Langevin Monte Carlo). The `doStepSizeAdapt` flag determines whether the kernel will automatically adapt its step size to achieve a target optimal acceptance ratio (defaults to `true`). Step size adaptation uses the algorithm described in [this paper](http://arxiv.org/abs/1111.4246). This kernel is more expensive per proposal than `qs.HARMKernel` or `qs.DriftKernel`, but it can still have much higher statistical efficiency for highly-constrained models.

`qs.MixtureKernel(kernels, weights)`  
Constructs a kernel that stochastically alternates between multiple different kernels. For example, to make a kernel that uses `qs.LARJKernel` to change structural variables 10% of the time and uses `qs.TraceMHKernel` to change nonstructural variables otherwise:

	qs.MixtureKernel({qs.LARJKernel({intervals=20}),
					  qs.TraceMHKernel({doStruct=false})},
					 {0.1, 0.9})

`qs.AnnealingKernel(kernel, annealingFn)`  
Performs simulated annealing by adjusting the *temperature* of the program (i.e. a scaling factor on its log probability) as MCMC runs. For example, a simple linear annealing schedule could be defined like:

	local maxTemp = 100
	local minTemp = 0.1
	qs.AnnealingKernel(qs.TraceMHKernel(),
					   terra(currIter: int, numIters: int)
					   	   var t = currIter/double(numIters) 
					   	   return (1.0-t)*maxTemp + t*minTemp
					   end)

Note that under simulated annealing, samples collected by MCMC are no longer representative of the true distribution defined by the program. They are still perfectly useful for finding modes (i.e. maxima), however, using e.g. the `qs.MAP` query defined in the next section.


## Queries

An inference *query* describes how you want `qs.infer` to interpret the samples it collects using one of the above methods. Again, you can author your own queries (see `qs/infer.t`), but Quicksand provides several common ones:

### qs.Samples

Just return the samples directly, leaving it up to you to process them further. You are responsible for freeing the memory of the returned vector (by calling `:destruct` on it).

### qs.Expectation

Compute the exepcted value of the program by averaging the samples. If the return type of the program is a struct type, then it must overload several operators: binary `+` and `-`, binary `*` (this should be an "inner product"), and multiplication and division by a real number. This query takes an optional boolean argument that, when `true`, will cause it to also compute and return the variance of the program:

	local mean, variance = qs.infer(p, qs.Expectation(true), [method])   

### qs.MAP

Returns the sample with the largest probability. If this is a struct type, then you are responsible for freeing its memory.

### qs.Autocorrelation

Computes the autocorrelation of the samples and returns it in a vector (you're responsible for its memory). You can optionally pass in "true" mean and variance values; otherwise, it will use the mean and variance of the samples themselves.

### qs.Histogram

Returns a normalized histogram of program return values. This only makes sense for programs with discrete return values. The histogram is stored in a hash map (see `qs/lib/hashmap.t`); again, you're responsible for freeing its memory. If the return type of the program is a struct, then it must have the `__hash` method defined.


# More Examples

You can find more examples of probabilistic programs over at the [Forest respository](http://forestdb.org/) for generative models. Most of these programs are written in Church (a LISP-based probabilistic programming language), but nearly all of the same principles apply. The online [Probabilistic Models of Cognition](https://probmods.org/) course is also a great source for more examples and explanation.


# Acknowledgments

Quicksand is being developed as part of the [DARPA Probabilistic Programming for Advanced Machine Learning (PPAML)](http://www.darpa.mil/Our_Work/I2O/Programs/Probabilistic_Programming_for_Advanced_Machine_Learning_(PPAML).aspx) program. This material is based on research sponsored by DARPA under agreement number FA8750-14-2-0009. The U.S. Government is authorized to reproduce and distribute reprints for Governmental purposes notwithstanding any copyright notation thereon. The views and conclusions contained herein are those of the authors and should not be interpreted as necessarily representing the official policies or endorsements, either expressed or implied, of DARPA or the U.S. Government.

