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


