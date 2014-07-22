local qs = terralib.require("qs")
local S = terralib.require("qs.lib.std")
local D = terralib.require("qs.distrib")
local M = terralib.require("qs.lib.tmath")

local C = terralib.includecstring [[
#include <stdio.h>
inline void flush() { fflush(stdout); }
]]

-----------------------------------------------------------------------------------------

local testsRun = global(uint, 0)
local testsPassed = global(uint, 0)

-----------------------------------------------------------------------------------------

local assertEq = macro(function(name, testVal, trueVal)
	assert(testVal:gettype() == trueVal:gettype())
	local failureFormatStr
	if testVal:gettype():isintegral() then
		failureFormatStr = "failed! Value was %d, should have been %d\n"
	elseif testVal:gettype():isfloat() then
		failureFormatStr = "failed! Value was %g, should have been %g\n"
	else
		error("assertEq: Unsupported type " .. tostring(testVal:gettype()))
	end
	return quote
		S.printf("test: %s...", name)
		C.flush()
		testsRun = testsRun + 1
		var testv = testVal
		var truev = trueVal
		if M.fabs(double(testv-truev)) > 1e-8 then
			S.printf(failureFormatStr, testv, truev)
		else
			S.printf("passed.\n")
			testsPassed = testsPassed + 1
		end
	end
end)

-----------------------------------------------------------------------------------------

-- Just verify that a function compiles and runs
local function compileAndRunTest(name, fn)
	io.write(string.format("test: %s...", name))
	io.flush()
	testsRun:set(testsRun:get() + 1)
	local success, ret = pcall(fn)
	if not success then
		print("failed! Error message:")
		print("----------------------------------")
		print(ret)
		print("----------------------------------")
	else
		print("passed.")
		testsPassed:set(testsPassed:get() + 1)
		if ret.destruct then
			ret:destruct()
		end
	end
end

-- Verify that a function does *not* compile and run
-- (for testing error-catching)
local function failToCompileTest(name, fn)
	io.write(string.format("test: %s...", name))
	io.flush()
	testsRun:set(testsRun:get() + 1)
	local success, ret = pcall(fn)
	if not success then
		print("passed.")
		testsPassed:set(testsPassed:get() + 1)
	else
		print("failed! Program compiled when it should have failed.")
		if ret.destruct then
			ret:destruct()
		end
	end
end

-- Default parameters for expected value tests
local _numsamps = 150
local _lag = 20
local _runs = 5
local _errtol = 0.07

-- Check that a computed expected value is close enough to a known true value
local function expectedValueTest(name, prog, trueExp, method, optparams)
	io.write(string.format("test: %s...", name))
	testsRun:set(testsRun:get() + 1)

	optparams = optparams or {}
	local numsamps = optparams.numsamps or _numsamps
	local runs = optparams.runs or _runs
	local errtol = optparams.errtol or _errtol

	local methodfn
	if method == qs.ForwardSample or method == qs.WeightedRejectionSample then
		methodfn = method(numsamps)
	elseif method == qs.MCMC then
		local kernel = optparams.kernel or qs.TraceMHKernel()
		local params = {numsamps=numsamps, lag=(optparams.lag or _lag)}
		methodfn = method(kernel, params)
	end
	local inferfn = qs.infer(prog, qs.Expectation(), methodfn)

	local estimates = terralib.newlist()
	for i=1,runs do
		estimates:insert(inferfn())
	end
	local testmean = 0.0
	local meanerr = 0.0
	for _,x in ipairs(estimates) do
		testmean = testmean + x
		meanerr = meanerr + math.abs(x - trueExp)
	end
	testmean = testmean / #estimates
	meanerr = meanerr / #estimates
	if meanerr > errtol then
		print(string.format("failed! Expected value was %g, should have been %g", testmean, trueExp))
	else
		print("passed.")
		testsPassed:set(testsPassed:get() + 1)
	end
end

-- Run an expected value test on a program for all three methods
local function multiMethodExpectedValueTest(name, prog, trueExp, optparams)
	expectedValueTest(name .. " (forward)", prog, trueExp, qs.ForwardSample, optparams)
	expectedValueTest(name .. " (reject)", prog, trueExp, qs.WeightedRejectionSample, optparams)
	expectedValueTest(name .. " (mcmc)", prog, trueExp, qs.MCMC, optparams)
end

-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------

print("starting tests...")

local terra testLogprobFunctions()
	assertEq("bernoulli lp (1)", [D.bernoulli(double)].logprob(true, 0.3), M.log(0.3))
	assertEq("bernoulli lp (2)", [D.bernoulli(double)].logprob(false, 0.3), M.log(0.7))
	assertEq("uniform lp", [D.uniform(double)].logprob(0.5, 0.0, 2.0), -M.log(2.0))
	assertEq("gaussian lp (1)", [D.gaussian(double)].logprob(0.0, 0.1, 0.5), -0.2457913526447274)
	assertEq("gaussian lp (2)", [D.gaussian(double)].logprob(0.25, 0.1, 0.5), -0.27079135264472737)
	assertEq("gaussian lp (3)", [D.gaussian(double)].logprob(0.6, 0.1, 0.5), -0.7257913526447274)
	assertEq("gamma lp (1)", [D.gamma(double)].logprob(1.0, 2.0, 2.0), -1.8862944092546166)
	assertEq("gamma lp (2)", [D.gamma(double)].logprob(4.0, 2.0, 2.0), -2.000000048134726)
	assertEq("gamma lp (3)", [D.gamma(double)].logprob(8.0, 2.0, 2.0), -3.306852867574781)
	assertEq("beta lp (1)", [D.beta(double)].logprob(0.1, 2.0, 5.0), 0.677170196389683)
	assertEq("beta lp (2)", [D.beta(double)].logprob(0.2, 2.0, 5.0), 0.899185234324094)
	assertEq("beta lp (3)", [D.beta(double)].logprob(0.6, 2.0, 5.0), -0.7747911992475776)
	assertEq("binomial lp (1)", [D.binomial(double)].logprob(15, 0.5, 40), -3.3234338674089985)
	assertEq("binomial lp (2)", [D.binomial(double)].logprob(20, 0.5, 40), -2.0722579911387817)
	assertEq("binomial lp (3)", [D.binomial(double)].logprob(30, 0.5, 40), -7.2840211276953575)
	assertEq("poisson lp (1)", [D.poisson(double)].logprob(2, 4.0), -1.9205584583201643)
	assertEq("poisson lp (2)", [D.poisson(double)].logprob(5, 4.0), -1.8560199371825927)
	assertEq("poisson lp (3)", [D.poisson(double)].logprob(7, 4.0), -2.821100833226181)
	assertEq("multinomial lp (1)", [D.multinomial_array(3)(double)].logprob(0, array(0.2, 0.6, 0.2)), M.log(0.2))
	assertEq("multinomial lp (2)", [D.multinomial_array(3)(double)].logprob(1, array(0.2, 0.6, 0.2)), M.log(0.6))
	assertEq("multinomial lp (3)", [D.multinomial_array(3)(double)].logprob(2, array(0.2, 0.6, 0.2)), M.log(0.2))
	var mp = [S.Vector(double)].salloc():init(); mp:insert(0.2); mp:insert(0.6); mp:insert(0.2)
	assertEq("multinomial lp (4)", [D.multinomial_vector(double)].logprob(0, mp), M.log(0.2))
	assertEq("multinomial lp (5)", [D.multinomial_vector(double)].logprob(1, mp), M.log(0.6))
	assertEq("multinomial lp (6)", [D.multinomial_vector(double)].logprob(2, mp), M.log(0.2))
	assertEq("dirichlet lp (1)", [D.dirichlet_array(3)(double)].logprob(array(0.6, 0.3, 0.1), array(1.0, 1.0, 1.0)), 0.693147180559945)
	var dv = [S.Vector(double)].salloc():init(); dv:insert(0.6); dv:insert(0.3); dv:insert(0.1)
	var dp = [S.Vector(double)].salloc():init(); dp:insert(1.0); dp:insert(1.0); dp:insert(1.0)
	assertEq("dirichlet lp (2)", [D.dirichlet_vector(double)].logprob(dv, dp), 0.693147180559945)
end
testLogprobFunctions()


multiMethodExpectedValueTest(
"flip expectation",
qs.program(function()
	return terra()
		return qs.flip(0.7, {struc=false})
	end
end), 0.7)

multiMethodExpectedValueTest(
"uniform expectation",
qs.program(function()
	return terra()
		return qs.uniform(0.1, 0.4, {struc=false})
	end
end), 0.5*(.1+.4))

multiMethodExpectedValueTest(
"multinomial expectation (1)",
qs.program(function()
	return terra()
		var items = array(0.2, 0.3, 0.4)
		return items[qs.multinomial(array(0.2, 0.6, 0.2), {struc=false})]
	end
end), 0.2*.2 + 0.6*.3 + 0.2*.4)

multiMethodExpectedValueTest(
"multinomial expectation (2)",
qs.program(function()
	return terra()
		var items = array(0.2, 0.3, 0.4)
		var params = [S.Vector(double)].salloc():init()
		params:insert(0.2); params:insert(0.6); params:insert(0.2)
		return items[qs.multinomial(params, {struc=false})]
	end
end), 0.2*.2 + 0.6*.3 + 0.2*.4)




if testsPassed:get() == testsRun:get() then
	print(string.format("all %d tests passed!", testsRun:get()))
else
	print(string.format("SOME TESTS FAILED!!! (passed %d/%d)",
		testsPassed:get(), testsRun:get()))
end







