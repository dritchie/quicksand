local util = require("qs.lib.util")

local S = require("qs.lib.std")
local tmath = require("qs.lib.tmath")
local qs = require("qs.globals")
local trace = require("qs.trace")




-- The number of parameters required by a random choice
local function getNumParams(sl)
	return #sl.sample:gettype().parameters
end

-- Default proposal for when no custom one is provided
-- (Just resamples from the prior)
local function makeDefaultProposal(sl)
	local ArgTypes = sl.logprob:gettype().parameters
	local ValType = ArgTypes[1]
	local ParamTypes = terralib.newlist()
	for i=2,#ArgTypes do ParamTypes:insert(ArgTypes[i]) end
	local currval = symbol(ValType)
	local params = ParamTypes:map(function(pt) return symbol(pt) end)
	local SampleValType = sl.sample:gettype().returntype
	return terra([currval], [params])
		var newval = sl.sample([params])
		var fwdlp = sl.logprob(escape
			if ValType == &SampleValType then
				emit `&newval
			else
				emit `newval
			end
		end, [params])
		var rvslp = sl.logprob(currval, [params])
		return newval, fwdlp, rvslp
	end
end




-- Random choices can be passed an optional struct with extra arguments
-- The following functions assume that the struct is passed in as a 
--    typed quote.
local function structHasMember(s, name)
	if not s then return false end
	local t = s:gettype()
	for _,e in ipairs(t.entries) do
		if e.field == name then
			return true
		end
	end
	return false
end
-- These are the options we consider
local Options =
{
	Structural = "struc",	-- whether the variable is structural or not
	InitialVal = "init"     -- value to initialize the variable with
}
-- The Structural option must have a compile-time constant (i.e. true or false) value
local function getStructuralOption(s)
	-- All variables are structural by default
	if not s then return true end
	local t = s:gettype()
	local index = 0
	for i,e in ipairs(t.entries) do
		if e.field == Options.Structural then
			index = i
			break
		end
	end
	if index > 0 then
		local value = s.tree.expression.expressions[index].value
		assert(value ~= nil and (type(value) == "boolean" or type(value.object) == "cdata"),
			string.format("'%s' option for a random choice must be a boolean compile-time constant", Options.Structural))
		if type(value) ~= "boolean" then
			local num = tonumber(value.object)
			if num == 0 then value = false else value = true end
		end
		return value
	else
		-- All variables are structural by default
		return true
	end
end

-- Defining bounding behavior for continuous random choices
-- FYI: My notion of forward/inverse is flipped from what's in the Stan manual
local logistic = macro(function(x)
	return `1.0 / (1.0 + tmath.exp(-x))
end)
local invlogistic = macro(function(y)
	return `-tmath.log(1.0/y - 1.0)
end)
local makeArrayLikeFrom = macro(function(origThing)
	local T = origThing:gettype()
	if T:isarray() then
		local nums = terralib.newlist()
		for i=1,T.N do nums:insert(`[T.type](0.0)) end
		return `arrayof([T.type], [nums])
	else
		return quote
			var thing = T.salloc():init(origThing:size())
			thing._size = origThing:size()
		in
			@thing
		end
	end
end)
local length = macro(function(thing)
	local T = thing:gettype()
	if T:isarray() then
		return `[T.N]
	else
		return `thing:size()
	end
end)
local get = macro(function(thing, i)
	local T = thing:gettype()
	if T:isarray() then
		return `thing[i]
	else
		return `thing(i)
	end
end)
local Bounds
Bounds =
{
	None = 
	{
		forwardTransform = macro(function(x) return x end),
		inverseTransform = macro(function(y) return y end),
		logprobIncrement = macro(function(x) return `0.0 end),
		getBoundingParams = S.memoize(function(real)
			return macro(function(...) return quote end end)
		end)
	},

	Lower = function(getLowerBoundFn)
		return
		{
			forwardTransform = macro(function(x, lo)
				return `tmath.fmax(tmath.exp(x) + lo, lo+1e-15)
			end),
			inverseTransform = macro(function(y, lo)
				local z = `tmath.fmax(y, lo + 1e-15)
				return `tmath.log(z - lo)
			end),
			logprobIncrement = macro(function(x, lo)
				return x
			end),
			getBoundingParams = getLowerBoundFn
		}
	end,

	Upper = function(getUpperBoundFn)
		return
		{
			forwardTransform = macro(function(x, hi)
				return `tmath.fmin(hi - tmath.exp(x), hi-1e-15)
			end),
			inverseTransform = macro(function(y, hi)
				local z = `tmath.fmin(y, hi - 1e-15)
				return `tmath.log(hi - z)
			end),
			logprobIncrement = macro(function(x, hi)
				return x
			end),
			getBoundingParams = getUpperBoundFn
		}
	end,

	LowerUpper = function(getBoundsFn)
		return
		{
			forwardTransform = macro(function(x, lo, hi)
				return quote
					var logit = logistic(x)
					if x > [-math.huge] and logit == 0.0 then logit = 1e-15 end
					if x < [math.huge] and logit == 1.0 then logit = [1.0 - 1e-15] end
					var y = lo + (hi - lo) * logit
				in
					y
				end
			end),
			inverseTransform = macro(function(y, lo, hi)
				local z = `tmath.fmax(tmath.fmin(y, hi - 1e-15), lo + 1e-15)
				local t = `(z - lo) / (hi - lo)
				return `invlogistic(t)
			end),
			logprobIncrement = macro(function(x, lo, hi)
				return `tmath.log(hi - lo) - x - 2.0*tmath.log(1.0 + tmath.exp(-x))
			end),
			getBoundingParams = getBoundsFn
		}
	end,

	UnitSimplex =
	{
		forwardTransform = macro(function(x)
			local real = x:gettype().type
			return quote
				var y = makeArrayLikeFrom(x)
				var Km1 = length(x)-1
				var sticklen = real(1.0)
				for k=0,Km1 do
					var z = logistic(get(x,k) - tmath.log(Km1 - k))
					get(y,k) = sticklen*z
					sticklen = sticklen - get(y,k)
				end
				get(y,Km1) = sticklen
			in
				y
			end
		end),
		inverseTransform = macro(function(y)
			return quote
				var x = makeArrayLikeFrom(y)
				var Km1 = length(y)-1
				var sticklen = get(y,Km1)
				var sum = get(y,0)
				for i=1,Km1+1 do sum = sum + get(y,i) end
				for k=Km1-1,-1,-1 do
					sticklen = sticklen + get(y,k)
					var z = get(y,k) / sticklen
					get(x,k) = invlogistic(z) + tmath.log(Km1 - k)
				end
			in
				x
			end
		end),
		logprobIncrement = macro(function(x)
			local real = x:gettype().type
			return quote
				var lp = real(0.0)
				var Km1 = length(x)-1
				var sticklen = real(1.0)
				for k=0,Km1 do
					var z = logistic(get(x,k) - tmath.log(Km1 - k))
				  	lp = lp + tmath.log(sticklen)
				  	lp = lp + tmath.log(z)
				  	lp = lp + tmath.log(1.0 - z)
					sticklen = sticklen - (sticklen*z)
				end
			in
				lp
			end
		end),
		getBoundingParams = S.memoize(function(real)
			return macro(function(...) return quote end end)
		end)
	}
}





-- Generate an S.copy statement when the second argument may be pointer-to-struct
local ptrSafeCopy = macro(function(self, other)
	return quote
		S.copy(self, [(other:gettype() == &self:gettype()) and (`@other) or other])
	end
end)




local function makeRandomChoice(sampleAndLogprob, proposal, bounding)

	bounding = bounding or Bounds.None

	-- The RandomChoice struct records the parameters and value of the random choice
	--    in the execution trace.
	local RandomChoice
	RandomChoice = S.memoize(function(real, isStructural)

		local sl = sampleAndLogprob(real)
		local propose = proposal and proposal(real) or makeDefaultProposal(sl)

		local function paramField(i) return string.format("param%d", i) end

		-- Declare struct type and add members for value, params, and bounds (if needed)
		local struct RandomChoiceT(trace.Object)
		{
			logprob: real,
			active: bool, 		 -- Was this choice reachable in last run of program?
			needsRescore: bool,  -- Do we need to recalculate the prior probability of this choice?
								 --   (because external code i.e. changed its value)
			lexid: uint 		 -- Lexically-unique ID
		}
		local ValueType = sl.sample:gettype().returntype
		assert(util.isPOD(ValueType) or ValueType:getmethod("copy"),
			"Non-POD value type for a random choice must have a 'copy' initialization method")
		local ParamTypes = sl.sample:gettype().parameters
		-- Parameters that are pointer-to-struct are stored as struct value types
		local StoredParamTypes = ParamTypes:map(function(pt) if pt:ispointertostruct() then return pt.type else return pt end end)
		-- We also verify that any non-POD parameter types have a __copy method
		--    (otherwise memory bugs will arise)
		for _,spt in ipairs(StoredParamTypes) do
			if not util.isPOD(spt) then
				assert(spt:getmethod("copy"),
					"Non-POD parameters to a random choice must have a 'copy' initialization method")
			end
		end
		RandomChoiceT.entries:insert({field="value", type=ValueType})
		for i,spt in ipairs(StoredParamTypes) do
			RandomChoiceT.entries:insert({field=paramField(i), type=spt})
		end

		function RandomChoiceT.withRealType(real) return RandomChoice(real, isStructural) end

		-- Store some properties on the type that other code might want to refer to
		RandomChoiceT.RealType = real
		RandomChoiceT.ValueType = ValueType
		RandomChoiceT.isStructural = isStructural
		RandomChoiceT.sampleFunction = sl.sample

		local function paramArgList(self)
			local lst = terralib.newlist()
			for i=1,#ParamTypes do
				if ParamTypes[i] == &StoredParamTypes[i] then
					lst:insert(`&self.[paramField(i)])
				else
					lst:insert(`self.[paramField(i)])
				end
			end
			return lst
		end

		-- Bounding transform functions
		local getBoundingParams = bounding.getBoundingParams(real)
		local function fwd(x, self)
			return quote
				var boundParams = getBoundingParams([paramArgList(self)])
			in
				bounding.forwardTransform(x, unpacktuple(boundParams))
			end
		end
		local function inv(y, self)
			return quote
				var boundParams = getBoundingParams([paramArgList(self)])
			in
				bounding.inverseTransform(y, unpacktuple(boundParams))
			end
		end
		local function lpincr(x, self)
			return quote
				var boundParams = getBoundingParams([paramArgList(self)])
			in
				bounding.logprobIncrement(x, unpacktuple(boundParams))
			end
		end
		-- Whether bounds are actually applied depends on whether we're doing AD
		-- TODO: Should we just always apply bounds? It means we pay a computational cost
		--    when we use bounded variables with non-HMC kernels, but with the current
		--    approach, we're paying the cost of transforming all variables when we copy
		--    them between normal traces and HMC dual traces...
		local applyingBounds = (real == qs.dualnum)
		-- local applyingBounds = true
		



		-- Get the value of this random choice
		-- (A macro, because fwd may need to salloc() an object)
		RandomChoiceT.methods.getValue = macro(function(self)
			if applyingBounds then
				return fwd(`self.value, self)
			else
				return `self.value
			end
		end)

		-- Set the value of this random choice
		terra RandomChoiceT:setValue(newval: ValueType)
			S.rundestructor(self.value)
			escape
				if applyingBounds then
					emit quote
						newval = [inv(newval, self)]
					end
				end
			end
			S.copy(self.value, newval)
			self.needsRescore = true
		end
		RandomChoiceT.methods.setValue:setinlined(true)

		-- Get the unbounded (i.e. inverse-transformed) value of this random choice
		-- (A macro, because inv may need to salloc() an object)
		RandomChoiceT.methods.getUnboundedValue = macro(function(self)
			if applyingBounds then
				return `self.value
			else
				return inv(`self.value, self)
			end
		end)

		-- Set the unbounded value of this random choice
		terra RandomChoiceT:setUnboundedValue(newval: ValueType)
			S.rundestructor(self.value)
			escape
				if not applyingBounds then
					emit quote
						newval = [fwd(newval, self)]
					end
				end
			end
			S.copy(self.value, newval)
			self.needsRescore = true
		end
		RandomChoiceT.methods.setUnboundedValue:setinlined(true)

		-- Get the raw, stored value of this random choice.
		terra RandomChoiceT:getStoredValue()
			return self.value
		end
		RandomChoiceT.methods.getStoredValue:setinlined(true)

		-- Set the raw, stored value of this random choice.
		terra RandomChoiceT:setStoredValue(newval: ValueType)
			S.rundestructor(self.value)
			S.copy(self.value, newval)
			self.needsRescore = true
		end
		RandomChoiceT.methods.setStoredValue:setinlined(true)


		-- === Now a bunch of methods like the above, except for extracting
		-- ===    all real-valued components of the choice's value.
		-- === This is mostly for dealing with vector-valued variables.

		local function genGetRealComps(self, val, comps)
			if ValueType == real then
				return quote
					comps:insert(val)
					return 1
				end
			elseif ValueType:isarray() and ValueType.type == real then
				return quote
					for i=0,[ValueType.N] do comps:insert(val[i]) end
					return [ValueType.N]
				end
			elseif ValueType == S.Vector(real) then
				return quote
					for x in val do comps:insert(x) end
					return val:size()
				end
			elseif ValueType:isstruct() and ValueType:getmethod("getRealComponents") then
				return quote
					return val:getRealComponents(comps)
				end
			else
				return quote end
			end
		end

		local function genSetRealComps(self, comps, startindex, setter)
			if ValueType == real then
				return quote
					setter(self, comps(startindex))
					self.needsRescore = true
					return 1
				end
			elseif ValueType:isarray() and ValueType.type == real then
				return quote
					var val : ValueType
					for i=0,[ValueType.N] do
						val[i] = comps(startindex + i)
					end
					setter(self, val)
					self.needsRescore = true
					return [ValueType.N]
				end
			elseif ValueType == S.Vector(real) then
				return quote
					var val = ValueType.salloc():copy(&self.value)
					for i=0,self.value:size() do
						val(i) = comps(startindex + i)
					end
					setter(self, @val)
					self.needsRescore = true
					return val:size()
				end
			elseif ValueType:isstruct() and ValueType:getmethod("getRealComponents") then
				return quote
					var val = ValueType.salloc():copy(&self.value)
					var numset = val:setRealComponents(comps)
					seter(self, @val)
					self.needsRescore = true
					return numset
				end
			else
				return quote end
			end
		end

		terra RandomChoiceT:getRealComps(comps: &S.Vector(real))
			var val = self:getValue()
			[genGetRealComps(self, val, comps)]
		end

		terra RandomChoiceT:setRealComps(comps: &S.Vector(real), index: uint64)
			[genSetRealComps(self, comps, index, RandomChoiceT.methods.setValue)]
		end

		terra RandomChoiceT:getStoredRealComps(comps: &S.Vector(real))
			var val = self:getStoredValue()
			[genGetRealComps(self, val, comps)]
		end

		terra RandomChoiceT:setStoredRealComps(comps: &S.Vector(real), index: uint64)
			[genSetRealComps(self, comps, index, RandomChoiceT.methods.setStoredValue)]
		end

		terra RandomChoiceT:getUnboundedRealComps(comps: &S.Vector(real))
			var val = self:getUnboundedValue()
			[genGetRealComps(self, val, comps)]
		end

		terra RandomChoiceT:setUnboundedRealComps(comps: &S.Vector(real), index: uint64)
			[genSetRealComps(self, comps, index, RandomChoiceT.methods.setUnboundedValue)]
		end


		-- To be 100% correct, we also make sure that stored values are transformed correctly
		--    when we do copyFromRealType.
		function RandomChoiceT.__copyFromRealType(realType)
			return terra(self: &RandomChoiceT, other: &RandomChoiceT.withRealType(realType))
				[RandomChoiceT.copyMembersFromRealType(realType)](self, other)
				var otherval = other:getValue()
				var myval : ValueType
				[trace.copyFromRealType(real)](myval, otherval)
				self:setValue(myval)
				S.rundestructor(myval)
			end
		end


		-- Constructor 1: Has an initial value
		local paramSyms = ParamTypes:map(function(pt) return symbol(pt) end)
		local initValSym = symbol(util.isPOD(ValueType) and ValueType or &ValueType)
		terra RandomChoiceT:__init([paramSyms], lexid: uint, [initValSym]) : {}
			escape
				for i=1,#ParamTypes do
					emit quote ptrSafeCopy(self.[paramField(i)], [paramSyms[i]]) end
				end
			end
			var val = [util.isPOD(ValueType) and initValSym or (`@initValSym)]
			escape
				if applyingBounds then
					emit quote S.copy(self.value, [inv(val, self)]) end
				else
					emit quote S.copy(self.value, val) end
				end
			end
			self:rescore()
			self.active = true
			self.lexid = lexid
		end

		-- Constructor 2: Has no initial value
		paramSyms = ParamTypes:map(function(pt) return symbol(pt) end)
		terra RandomChoiceT:__init([paramSyms], lexid: uint) : {}
			-- Draw a sample, then call the other constructor
			var sampledval = sl.sample([paramSyms])
			self:__init([paramSyms], lexid,
				        [util.isPOD(ValueType) and sampledval or (`&sampledval)])
		end

		-- Update for a new run of the program, checking for parameter changes
		paramSyms = ParamTypes:map(function(pt) return symbol(pt) end)
		terra RandomChoiceT:update([paramSyms]) : {}
			var needsRescore = self.needsRescore
			escape
				-- If real is a dual-number type, then we must
				--    always update everything (otherwise memory bugs...)
				if real == qs.dualnum then
					emit quote needsRescore = true end
					for i=1,#ParamTypes do
						emit quote
							S.rundestructor(self.[paramField(i)])
							ptrSafeCopy(self.[paramField(i)], [paramSyms[i]])
						end
					end
				-- Otherwise, we only need to copy/rescore if we found something
				--    that's changed since last time.
				else
					for i=1,#ParamTypes do
						local p = `[paramSyms[i]]
						-- __eq operator takes value types
						if ParamTypes[i]:ispointertostruct() then p = `@p end
						emit quote
							if not util.equal(self.[paramField(i)], p) then
								needsRescore = true
								S.rundestructor(self.[paramField(i)])
								ptrSafeCopy(self.[paramField(i)], [paramSyms[i]])
							end
						end
					end
				end
			end
			if needsRescore then
				self:rescore()
			end
			self.active = true
		end

		-- Is this choice structural or not?
		-- (Structural-ness is a static property, but sometimes we need to query it at runtime for
		--    different choices)
		terra RandomChoiceT:getIsStructural() return isStructural end
		RandomChoiceT.methods.getIsStructural:setinlined(true)

		-- Retrieve lexically-unique ID
		terra RandomChoiceT:getLexicalID() return self.lexid end
		RandomChoiceT.methods.getLexicalID:setinlined(true)		

		-- Rescore by recomputing prior logprob
		terra RandomChoiceT:rescore() : {}
			var val = self:getValue()
			self.logprob = sl.logprob(escape
				if sl.logprob:gettype().parameters[1] == &ValueType then
					emit `&val
				else
					emit `val
				end
			end, [paramArgList(self)])
			escape
				if applyingBounds then
					emit quote
						self.logprob = self.logprob + [lpincr(`self:getStoredValue(), self)]
					end
				end
			end
			self.needsRescore = false
		end

		-- Propose new value, return fwd/rvs proposal probabilities
		terra RandomChoiceT:proposal() : {real, real}
			var newval : ValueType
			var fwdlp : real
			var rvslp : real
			var currval = self:getValue()
			newval, fwdlp, rvslp = propose(escape
				if propose:gettype().parameters[1] == &ValueType then
					emit `&currval
				else
					emit `currval
				end
			end, [paramArgList(self)])
			self:setValue(newval)
			return fwdlp, rvslp
		end

		return RandomChoiceT
	end)

	-- This macro is how the random choice is exposed to client code
	local erp = macro(function(...)
		local args = {...}
		local sl = sampleAndLogprob(qs.real)
		local n = getNumParams(sl)
		local N = #args
		assert(N == n or N == n+1, "Wrong number of parameters to random choice")
		local opts = nil
		if N == n+1 then
			opts = args[N]
			args[N] = nil
		end
		local isStructural = getStructuralOption(opts)
		local extraInitArgs = terralib.newlist()
		extraInitArgs:insert(trace.compilation.nextErpLexID())
		if structHasMember(opts, Options.InitialVal) then
			extraInitArgs:insert(`opts.[Options.InitialVal])
		end
		local RandomChoiceT = RandomChoice(qs.real, isStructural)
		---------------------
		return trace.lookupRandomChoiceValue(RandomChoiceT, args, extraInitArgs)
	end)

	-- This macro facilitates models where we directly observe the values
	--    of some random choices, instead of sampling them
	erp.observe = macro(function(value, ...)
		local args = {...}
		local sl = sampleAndLogprob(qs.real)
		local n = getNumParams(sl)
		local N = #args
		assert(N == n, "Wrong number of parameters to random choice observation")
		return quote
			trace.exports.factor(sl.logprob(value, [args]))
		end
	end)


	return erp

end


return
{
	makeRandomChoice = makeRandomChoice,
	Bounds = Bounds,
	structHasMember = structHasMember,
	exports = 
	{
		makeRandomChoice = makeRandomChoice
	}
}



