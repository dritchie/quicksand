local util = terralib.require("qs.lib.util")

local S = util.require("lib.std")
local tmath = util.require("lib.tmath")
local globals = util.require("globals")
local trace = util.require("trace")




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
local logistic = macro(function(x)
	return `1.0 / (1.0 + tmath.exp(-x))
end)
local invlogistic = macro(function(y)
	return `-tmath.log(1.0/y - 1.0)
end)
local Bounds =
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
				return `tmath.exp(x) + lo
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
				return `hi - tmath.exp(x)
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
	end

	-- TODO: Simplex bounds (for dirichlet)
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
	local RandomChoice = S.memoize(function(real, isStructural)

		local sl = sampleAndLogprob(real)
		local propose = proposal and proposal(real) or makeDefaultProposal(sl)

		local function paramField(i) return string.format("param%d", i) end

		-- Declare struct type and add members for value, params, and bounds (if needed)
		local struct RandomChoiceT(S.Object)
		{
			logprob: real,
			active: bool
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

		-- Bounds only take effect if we're doing HMC with dual numbers
		if real ~= globals.dualnum then bounding = Bounds.None end
		local getBoundingParams = bounding.getBoundingParams(real)
		local function fwd(x, self)
			return `bounding.forwardTransform(x, getBoundingParams([paramArgList(self)]))
		end
		local function inv(y, self)
			return `bounding.inverseTransform(y, getBoundingParams([paramArgList(self)]))
		end
		local function lpincr(x, self)
			return `bounding.logprobIncrement(x, getBoundingParams([paramArgList(self)]))
		end


		-- Constructor 1: Has an initial value
		local paramSyms = ParamTypes:map(function(pt) return symbol(pt) end)
		local initValSym = symbol(util.isPOD(ValueType) and ValueType or &ValueType)
		terra RandomChoiceT:__init([paramSyms], [initValSym]) : {}
			escape
				for i=1,#ParamTypes do
					emit quote ptrSafeCopy(self.[paramField(i)], [paramSyms[i]]) end
				end
			end
			ptrSafeCopy(self.value, [inv(initValSym, self)])
			self:rescore()
			self.active = true
		end

		-- Constructor 2: Has no initial value
		paramSyms = ParamTypes:map(function(pt) return symbol(pt) end)
		terra RandomChoiceT:__init([paramSyms]) : {}
			-- Draw a sample, then call the other constructor
			var sampledval = sl.sample([paramSyms])
			self:__init([paramSyms],
				        [util.isPOD(ValueType) and sampledval or (`&sampledval)])
		end

		-- Update for a new run of the program, checking for parameter changes
		paramSyms = ParamTypes:map(function(pt) return symbol(pt) end)
		terra RandomChoiceT:update([paramSyms]) : {}
			var hasChanges = false
			escape
				-- If real is a dual-number type, then we must
				--    always update everything (otherwise memory bugs...)
				if real == globals.dualnum then
					hasChanges = true
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
								hasChanges = true
								S.rundestructor(self.[paramField(i)])
								ptrSafeCopy(self.[paramField(i)], [paramSyms[i]])
							end
						end
					end
				end
			end
			if hasChanges then
				self:rescore()
			end
			self.active = true
		end

		-- Is this choice structural or not?
		-- (Structural-ness is a static property, but sometimes we need to query it at runtime for
		--    different choices)
		terra RandomChoiceT:isStructural() return isStructural end
		RandomChoiceT.methods.isStructural:setinlined(true)

		-- Rescore by recomputing prior logprob
		terra RandomChoiceT:rescore() : {}
			var val = self:getValue()
			self.logprob = sl.logprob(escape
				if sl.logprob:gettype().parameters[1] == &ValueType then
					emit `&val
				else
					emit `val
				end
			end, [paramArgList(self)]) + [lpincr(`self.value, self)]
		end

		-- Propose new value, return fwd/rvs proposal probabilities
		terra RandomChoiceT:proposal() : {real, real}
			var fwdlp : real
			var rvslp : real
			var currval = self:getValue()
			self.value, fwdlp, rvslp = propose(escape
				if propose:gettype().parameters[1] == &ValueType then
					emit `&currval
				else
					emit `currval
				end
			end, [paramArgList(self)])
			S.rundestructor(currval)
			self.value = [inv(`self.value, self)]
			self:rescore()
			return fwdlp, rvslp
		end

		-- Get the (transformed) stored value of this random choice
		terra RandomChoiceT:getValue()
			return [fwd(`self.value, self)]
		end
		RandomChoiceT.methods.getValue:setinlined(true)


		-- TODO later(?):
		--  * setValue
		--  * getUntransformedValue / setUntransformedValue
		--  * getRealComponents / setRealComponents
		--  * getUntransformedRealComponents / setUntransformedRealComponents

		return RandomChoiceT
	end)

	-- This macro is how the random choice is exposed to client code
	local erp = macro(function(...)
		local args = {...}
		local sl = sampleAndLogprob(globals.real)
		local n = getNumParams(sl)
		local N = #args
		assert(N == n or N == n+1, "Wrong number of parameters to random choice")
		local opts = nil
		if N == n+1 then
			opts = args[N]
			args[N] = nil
		end
		local isStructural = getStructuralOption(opts)
		local initVal = nil
		if structHasMember(opts, Options.InitialVal) then
			initVal = (`opts.[Options.InitialVal])
		end
		local RandomChoiceT = RandomChoice(globals.real, isStructural)
		---------------------
		return trace.lookupRandomChoiceValue(RandomChoiceT, args, initVal)
	end)

	-- This macro facilitates models where we directly observe the values
	--    of some random choices, instead of sampling them
	erp.observe = macro(function(value, ...)
		local args = {...}
		local sl = sampleAndLogprob(globals.real)
		local n = getNumParams(sl)
		local N = #args
		assert(N == n, "Wrong number of parameters to random choice observation")
		return quote
			trace.factor(sl.logprob(value, [args]))
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



