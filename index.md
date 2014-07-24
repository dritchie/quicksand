---
layout: post
title: Quicksand
---

# Getting Started with Quicksand

Quicksand is a library for [probabilistic programming](http://web.stanford.edu/~ngoodman/papers/POPL2013-abstract.pdf) in the Terra programming language.


## Installation

You'll first need to download and build [Terra](http://terralang.org).

Then just add the following to your `.profile`, `.bashrc`, or equivalent file:

	export QS=[path to quicksand root]
	export LUA_PATH="$LUA_PATH;$QS/?.t/$QS/?/init.t"

Quicksand has been tested on OSX and should also work on Linux. On Windows, your mileage may vary.


## A Simple Quicksand Program

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
		end

	end)

Here, the program `p2` draws mixture paramters and mixture component means from Dirichlet and Gaussian priors, respectively. For each data point, it then selects which mixture component the data point was drawn from (a latent variable), and then accounts for the probability of drawing that data point from that mixture component (here, all components have a constant standard deviation of 1).