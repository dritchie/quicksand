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

	local p = qs.program(function()
		return terra()
			var a = qs.flip(0.5)
			var b = qs.flip(0.5)
			qs.condition(a or b)
			return (a and b)
		end
	end)

The probabilistic program `p` draws two random values via unbiased coin flips (the 0.5 means "50% chance of coming up true") and returns the logical AND of those two values, subject to the constraint that at least one of them is true. 
