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
		failureFormatStr = "FAILED! Value was %d, should have been %d\n"
	elseif testVal:gettype():isfloat() then
		failureFormatStr = "FAILED! Value was %g, should have been %g\n"
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
		print("FAILED! Error message:")
		print("----------------------------------")
		print(ret)
		print("----------------------------------")
	else
		print("passed.")
		testsPassed:set(testsPassed:get() + 1)
		if type(ret) == "cdata" and ret.destruct then
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
		print("FAILED! Program compiled when it should have failed.")
		if type(ret) == "cdata" and ret.destruct then
			ret:destruct()
		end
	end
end

-- Default parameters for expected value tests
local _numsamps = 150
local _lag = 20
local _runs = 5
local _errtol = 0.07
local LARJintervals = 20
local HMCsteps = 10

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
		local params = {numsamps=numsamps, lag=(optparams.lag or _lag), verbose=(optparams.verbose or false)}
		methodfn = method(kernel, params)
	end

	local estimates = terralib.newlist()
	local function getEstimates()
		local inferfn = qs.infer(prog, qs.Expectation(), methodfn)
		for i=1,runs do
			estimates:insert(inferfn())
		end
	end
	local success, err = pcall(getEstimates)
	if not success then
		print("FAILED! Error message:")
		print("----------------------------------")
		print(err)
		print("----------------------------------")
		return
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
		print(string.format("FAILED! Expected value was %g, should have been %g", testmean, trueExp))
	else
		print("passed.")
		-- print(string.format("passed (expected val: %g)", testmean))
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

qs.initrand()

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
		var params = [S.Vector(qs.real)].salloc():init()
		params:insert(0.2); params:insert(0.6); params:insert(0.2)
		return items[qs.multinomial(params, {struc=false})]
	end
end), 0.2*.2 + 0.6*.3 + 0.2*.4)

multiMethodExpectedValueTest(
"gaussian expectation",
qs.program(function()
	return terra()
		return qs.gaussian(0.1, 0.5, {struc=false})
	end
end), 0.1)

multiMethodExpectedValueTest(
"gamma expectation",
qs.program(function()
	return terra()
		return qs.gamma(2.0, 2.0, {struc=false})/10.0
	end
end), 0.4)

multiMethodExpectedValueTest(
"beta expectation",
qs.program(function()
	return terra()
		return qs.beta(2.0, 5.0, {struc=false})
	end
end), 2.0/(2+5))

multiMethodExpectedValueTest(
"binomial expectation",
qs.program(function()
	return terra()
		return qs.binomial(0.5, 40, {struc=false})/40.0
	end
end), 0.5)

multiMethodExpectedValueTest(
"poisson expectation",
qs.program(function()
	return terra()
		return qs.poisson(4.0, {struc=false})/10.0
	end
end), 0.4)

local dirichletArrayProg = qs.program(function()
	return terra()
		return qs.dirichlet(array(1.0, 1.0, 1.0, 1.0), {struc=false})
	end
end)
compileAndRunTest(
"dirichlet array compile and run (forward)",
function() return qs.infer(dirichletArrayProg, qs.Samples, qs.ForwardSample(_numsamps))() end)
compileAndRunTest(
"dirichlet array compile and run (reject)",
function() return qs.infer(dirichletArrayProg, qs.Samples, qs.WeightedRejectionSample(_numsamps))() end)
compileAndRunTest(
"dirichlet array compile and run (mcmc)",
function() return qs.infer(dirichletArrayProg, qs.Samples, qs.MCMC(qs.TraceMHKernel(), {numsamps=_numsamps}))() end)

local dirichletVectorProg = qs.program(function()
	return terra()
		var p = [S.Vector(qs.real)].salloc():init()
		p:insert(1.0); p:insert(1.0); p:insert(1.0); p:insert(1.0)
		var d : S.Vector(qs.real)
		d:copy(qs.dirichlet(p, {struc=false}))
		return d
	end
end)
compileAndRunTest(
"dirichlet vector compile and run (forward)",
function() return qs.infer(dirichletVectorProg, qs.Samples, qs.ForwardSample(_numsamps))() end)
compileAndRunTest(
"dirichlet vector compile and run (reject)",
function() return qs.infer(dirichletVectorProg, qs.Samples, qs.WeightedRejectionSample(_numsamps))() end)
compileAndRunTest(
"dirichlet vector compile and run (mcmc)",
function() return qs.infer(dirichletVectorProg, qs.Samples, qs.MCMC(qs.TraceMHKernel(), {numsamps=_numsamps}))() end)

compileAndRunTest(
"MAP compile and run",
function()
	return qs.infer(qs.program(function()
		return terra()
			return qs.gaussian(0.0, 1.0, {struc=false})
		end
	end),
	qs.MAP, qs.WeightedRejectionSample(_numsamps))()
end)

compileAndRunTest(
"autocorrelation compile and run",
function()
return qs.infer(qs.program(function()
		return terra()
			return qs.gaussian(0.0, 1.0, {struc=false})
		end
	end),
	qs.Autocorrelation(), qs.WeightedRejectionSample(_numsamps))()
end)

compileAndRunTest(
"histogram compile and run",
function()
return qs.infer(qs.program(function()
		return terra()
			return qs.multinomial(array(0.6, 0.3, 0.1), {struc=false})
		end
	end),
	qs.Histogram, qs.WeightedRejectionSample(_numsamps))()
end)

expectedValueTest(
"initial value expectation",
qs.program(function()
	return terra()
		return qs.gaussian(0.1, 0.5, {struc=false, init=0.2})
	end
end), 0.1, qs.MCMC)

failToCompileTest(
"non-constant struc tag error",
function()
return qs.infer(qs.program(function()
		return terra()
			var s = false
			return qs.gaussian(0.0, 1.0, {struc=s})
		end
	end),
	qs.Expectation(), qs.WeightedRejectionSample(_numsamps))()
end)

expectedValueTest(
"condition expectation (reject)",
qs.program(function()
	return terra()
		var a = qs.flip(0.5, {struc=false})
		qs.condition(a)
		return a
	end
end), 1.0, qs.WeightedRejectionSample)

expectedValueTest(
"condition expectation (mcmc)",
qs.program(function()
	return terra()
		var a = qs.flip(0.5, {struc=false})
		qs.condition(a)
		return a
	end
end), 1.0, qs.MCMC)

expectedValueTest(
"conditionfunc expectation",
qs.program(function()
	local cf = qs.conditionfunc(terra(x: int)
		return x == 2
	end)
	return terra()
		var x = qs.multinomial(array(0.6, 0.3, 0.1), {struc=false})
		cf(x)
		return x
	end
end), 2.0, qs.MCMC)

expectedValueTest(
"factor expectation (reject)",
qs.program(function()
	return terra()
		var x = qs.uniform(-1.0, 1.0, {struc=false})
		qs.factor([D.gaussian(qs.real)].logprob(x, 0.3, 0.1))
		return x
	end
end), 0.3, qs.WeightedRejectionSample)

expectedValueTest(
"factor expectation (mcmc)",
qs.program(function()
	return terra()
		var x = qs.uniform(-1.0, 1.0, {struc=false})
		qs.factor([D.gaussian(qs.real)].logprob(x, 0.3, 0.1))
		return x
	end
end), 0.3, qs.MCMC)

expectedValueTest(
"factorfunc expectation",
qs.program(function()
	local ff = qs.factorfunc(terra(x: qs.real)
		return [D.gaussian(qs.real)].logprob(x, 0.3, 0.1)
	end)
	return terra()
		var x = qs.uniform(-1.0, 1.0, {struc=false, init=0.0})
		ff(x)
		return x
	end
end), 0.3, qs.MCMC)

expectedValueTest(
"observe expectation (reject)",
qs.program(function()
	return terra()
		var x = qs.uniform(-1.0, 1.0, {struc=false})
		qs.gaussian.observe(x, 0.3, 0.1)
		return x
	end
end), 0.3, qs.WeightedRejectionSample)

expectedValueTest(
"observe expectation (mcmc)",
qs.program(function()
	return terra()
		var x = qs.uniform(-1.0, 1.0, {struc=false})
		qs.gaussian.observe(x, 0.3, 0.1)
		return x
	end
end), 0.3, qs.MCMC)

expectedValueTest(
"multiple choices expectation",
qs.program(function()
	return terra()
		var a = qs.flip(0.5, {struc=false})
		var b = qs.flip(0.5, {struc=false})
		qs.condition(a or b)
		return (a and b)
	end
end), 1.0/3.0, qs.MCMC)

expectedValueTest(
"basic control flow expectation",
qs.program(function()
	return terra()
		if qs.flip(0.7) then
			return qs.flip(0.2, {struc=false})
		else
			return qs.flip(0.8, {struc=false})
		end
	end
end), 0.7*0.2 + 0.3*0.8, qs.MCMC)

expectedValueTest(
"hierarchical flip expectation",
qs.program(function()
	return terra()
		var weight = 0.8
		if qs.flip(0.7) then weight = 0.2 end
		return qs.flip(weight, {struc=false})
	end
end), 0.7*0.2 + 0.3*0.8, qs.MCMC)

expectedValueTest(
"qs.func expectation",
qs.program(function()
	local helper = qs.func(terra()
		return qs.gaussian(0.1, 0.5, {struc=false})
	end)
	return terra()
		return helper()
	end
end), 0.1, qs.MCMC)


expectedValueTest(
"recursive qs.func expectation",
qs.program(function()
	local powerLaw = qs.func()
	powerLaw:define(terra(prob: qs.real, x: int) : int
		if qs.flip(prob) then
			return x
		else
			return powerLaw(prob, x+1)
		end
	end)
	return terra()
		var a = powerLaw(0.3, 1)
		return a < 5
	end
end), 0.7599, qs.MCMC)

expectedValueTest(
"transdimensional expectation",
qs.program(function()
	return terra()
		var a = 0.7
		if qs.flip(0.9) then
			a = qs.beta(1.0, 5.0, {struc=false})
		end
		var b = qs.flip(a, {struc=false})
		qs.condition(b)
		return a
	end
end), 0.417, qs.MCMC)

expectedValueTest(
"transdimensional expectation (LARJ)",
qs.program(function()
	return terra()
		var a = 0.7
		if qs.flip(0.9) then
			a = qs.beta(1.0, 5.0, {struc=false})
		end
		var b = qs.flip(a, {struc=false})
		qs.condition(b)
		return a
	end
end),
0.417,
qs.MCMC,
{kernel = qs.MixtureKernel(
	{
		qs.LARJKernel({intervals=LARJintervals}),
		qs.TraceMHKernel({doStruct=false})
	},
	{1.0/3.0, 2.0/3.0}
)})

expectedValueTest(
"for loop expectation",
qs.program(function()
	return terra()
		var accum = 0
		for i=0,4 do
			accum = accum + int(qs.flip(0.5, {struc=false}))
		end
		return accum / 4.0
	end
end), 0.5, qs.MCMC)

expectedValueTest(
"range-based for loop expectation",
qs.program(function()
	return terra()
		var accum = 0
		for i in qs.range(0,4) do
			accum = accum + int(qs.flip(0.5, {struc=false}))
		end
		return accum / 4.0
	end
end), 0.5, qs.MCMC)	


-- Module tests

local mod = qs.module(function()
	local g = qs.func(terra(m: qs.real, sd: qs.real)
		return qs.gaussian(m, sd, {struc=false}) 
	end)
	return { g = g }
end)

expectedValueTest(
"module use expectation",
qs.program(function()
	local m = mod:open()
	return terra()
		return m.g(0.1, 0.5)
	end
end), 0.1, qs.MCMC)	

failToCompileTest(
"module.open error",
function()
	local p = qs.program(function()
		local m = mod:open()
		return terra()
			return m.g(0.1, 0.5)
		end
	end)
	local m = mod:open()
end)

compileAndRunTest(
"module.openAs compile and run",
function()
	local p = qs.program(function()
		local m = mod:open()
		return terra()
			return m.g(0.1, 0.5)
		end
	end)
	local m = mod:openAs(p)
end)

failToCompileTest(
"module.openAs error",
function()
	local p = qs.program(function()
		return terra()
			return qs.flip(0.5)
		end
	end)
	local m = mod:openAs(p)
end)


-- HMC tests

expectedValueTest(
"gaussian expectation (Langevin)",
qs.program(function()
	return terra()
		return qs.gaussian(0.1, 0.5, {struc=false})
	end
end), 0.1, qs.MCMC,
{kernel=qs.HMCKernel()})

expectedValueTest(
"gaussian expectation (HMC)",
qs.program(function()
	return terra()
		return qs.gaussian(0.1, 0.5, {struc=false})
	end
end), 0.1, qs.MCMC,
{kernel=qs.HMCKernel({numSteps=HMCsteps})})

expectedValueTest(
"uniform expectation (Langevin)",
qs.program(function()
	return terra()
		return qs.uniform(0.1, 0.4, {struc=false})
	end
end), 0.5*(.1+.4), qs.MCMC,
{kernel=qs.HMCKernel()})

expectedValueTest(
"uniform expectation (HMC)",
qs.program(function()
	return terra()
		return qs.uniform(0.1, 0.4, {struc=false})
	end
end), 0.5*(.1+.4), qs.MCMC,
{kernel=qs.HMCKernel({numSteps=HMCsteps})})

expectedValueTest(
"gamma expectation (Langevin)",
qs.program(function()
	return terra()
		return qs.gamma(2.0, 2.0, {struc=false})/10.0
	end
end), 0.4, qs.MCMC,
{kernel=qs.HMCKernel()})

expectedValueTest(
"gamma expectation (HMC)",
qs.program(function()
	return terra()
		return qs.gamma(2.0, 2.0, {struc=false})/10.0
	end
end), 0.4, qs.MCMC,
{kernel=qs.HMCKernel({numSteps=HMCsteps})})

expectedValueTest(
"beta expectation (Langevin)",
qs.program(function()
	return terra()
		return qs.beta(2.0, 5.0, {struc=false})
	end
end), 2.0/(2+5), qs.MCMC,
{kernel=qs.HMCKernel()})

expectedValueTest(
"beta expectation (HMC)",
qs.program(function()
	return terra()
		return qs.beta(2.0, 5.0, {struc=false})
	end
end), 2.0/(2+5), qs.MCMC,
{kernel=qs.HMCKernel({numSteps=HMCsteps})})

compileAndRunTest(
"dirichlet array compile and run (HMC)",
function() return
qs.infer(
qs.program(function()
	return terra()
		return qs.dirichlet(arrayof(qs.real, 1.0, 1.0, 1.0, 1.0), {struc=false})
	end
end),
qs.Samples, qs.MCMC(qs.HMCKernel(), {numsamps=_numsamps}))() end)

compileAndRunTest(
"dirichlet vector compile and run (HMC)",
function() return
qs.infer(
qs.program(function()
	return terra()
		var p = [S.Vector(qs.real)].salloc():init()
		p:insert(1.0); p:insert(1.0); p:insert(1.0); p:insert(1.0)
		var d : S.Vector(qs.real)
		d:copy(qs.dirichlet(p, {struc=false}))
		return d
	end
end),
qs.Samples, qs.MCMC(qs.HMCKernel(), {numsamps=_numsamps}))() end)

expectedValueTest(
"hmc multiple variables expectation",
qs.program(function()
	return terra()
		var sum = qs.real(0.0)
		for i=0,10 do
			sum = sum + qs.gaussian(0.1, 0.5, {struc=false})
		end
		return sum/10.0
	end
end), 0.1, qs.MCMC,
{kernel=qs.HMCKernel()})

expectedValueTest(
"hmc structure change expectation",
qs.program(function()
	return terra()
		var n = 2
		if qs.flip(0.5) then n = 5 end
		var sum = qs.real(0.0)
		for i=0,n do 
			sum = sum + qs.gaussian(0.1, 0.5, {struc=false})
		end
		return sum/n
	end
end), 0.1, qs.MCMC,
{kernel = qs.MixtureKernel(
	{
		qs.TraceMHKernel({doNonstruct=false}),
		qs.HMCKernel()
	},
	{0.1, 0.9}
)})

expectedValueTest(
"hmc larj annealing expectation",
qs.program(function()
	return terra()
		var n = 2
		if qs.flip(0.5) then n = 5 end
		var sum = qs.real(0.0)
		for i=0,n do 
			sum = sum + qs.gaussian(0.1, 0.5, {struc=false})
		end
		return sum/n
	end
end), 0.1, qs.MCMC,
{kernel = qs.MixtureKernel(
	{
		qs.LARJKernel({annealKernel=qs.HMCKernel(), intervals=LARJintervals}),
		qs.HMCKernel()
	},
	{0.1, 0.9}
)})


-- HARM tests
expectedValueTest(
"harm expectation",
qs.program(function()
	return terra()
		var sum = qs.real(0.0)
		for i=0,10 do
			sum = sum + qs.gaussian(0.1, 0.5, {struc=false})
		end
		return sum/10.0
	end
end), 0.1, qs.MCMC,
{kernel=qs.HARMKernel()})

expectedValueTest(
"harm structure change expectation",
qs.program(function()
	return terra()
		var n = 2
		if qs.flip(0.5) then n = 5 end
		var sum = qs.real(0.0)
		for i=0,n do 
			sum = sum + qs.gaussian(0.1, 0.5, {struc=false})
		end
		return sum/n
	end
end), 0.1, qs.MCMC,
{kernel = qs.MixtureKernel(
	{
		qs.TraceMHKernel({doNonstruct=false}),
		qs.HARMKernel()
	},
	{0.1, 0.9}
)})


-- Drift tests
expectedValueTest(
"drift expectation",
qs.program(function()
	return terra()
		var sum = qs.real(0.0)
		for i=0,10 do
			sum = sum + qs.gaussian(0.1, 0.5, {struc=false})
		end
		return sum/10.0
	end
end), 0.1, qs.MCMC,
{kernel=qs.DriftKernel()})

expectedValueTest(
"drift structure change expectation",
qs.program(function()
	return terra()
		var n = 2
		if qs.flip(0.5) then n = 5 end
		var sum = qs.real(0.0)
		for i=0,n do 
			sum = sum + qs.gaussian(0.1, 0.5, {struc=false})
		end
		return sum/n
	end
end), 0.1, qs.MCMC,
{kernel = qs.MixtureKernel(
	{
		qs.TraceMHKernel({doNonstruct=false}),
		qs.DriftKernel()
	},
	{0.1, 0.9}
)})

expectedValueTest(
"drift expectation (lexical scale sharing)",
qs.program(function()
	return terra()
		var sum = qs.real(0.0)
		for i=0,10 do
			sum = sum + qs.gaussian(0.1, 0.5, {struc=false})
		end
		return sum/10.0
	end
end), 0.1, qs.MCMC,
{kernel=qs.DriftKernel({lexicalScaleSharing=true})})

expectedValueTest(
"drift structure change expectation (lexical scale sharing)",
qs.program(function()
	return terra()
		var n = 2
		if qs.flip(0.5) then n = 5 end
		var sum = qs.real(0.0)
		for i=0,n do 
			sum = sum + qs.gaussian(0.1, 0.5, {struc=false})
		end
		return sum/n
	end
end), 0.1, qs.MCMC,
{kernel = qs.MixtureKernel(
	{
		qs.TraceMHKernel({doNonstruct=false}),
		qs.DriftKernel({lexicalScaleSharing=true})
	},
	{0.1, 0.9}
)})





if testsPassed:get() == testsRun:get() then
	print(string.format("all %d tests passed!", testsRun:get()))
else
	print(string.format("SOME TESTS FAILED!!! (passed %d/%d)",
		testsPassed:get(), testsRun:get()))
end







