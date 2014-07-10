local S = terralib.require("lib.std")
local tmath = terralib.require("lib.math")
local util = terralib.require("lib.util")
local qs = terralib.require("globals")


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
	return terra(currval, [params])
		var newval = sl.sample([params])
		var fwdlp = sl.logprob(escape
			if ValType == &SampleValType then
				emit `&newval
			else
				emit `newval
			end
		end, [args])
		var rvslp = sl.logprob(currval, [args])
		return newval, fwdlp, rvslp
	end
end



-- Random choices can be passed an optional struct with extra arguments
-- The following functions assume that the struct is passed in as a 
--    typed quote.
local function structHasMember(s, name)
	local t = s:gettype()
	for _,e in ipairs(t.entries) do
		if e.efield == name then
			return true
		end
	end
	return false
end
-- These are the options we consider
local Options = {
	Structural = "struc",	-- whether the variable is structural or not
	InitialVal = "init",    -- value to initialize the variable with
	LowerBound = "lo",		-- lower bound on the variable's value
	UpperBound = "hi",		-- upper bound on the variable's value
}
-- The Structural option must have a constant (i.e. true or false) value
local function getStructuralOption(s)
	local t = s:gettype()
	local index = 0
	for i,e in ipairs(t.entries) do
		if e.efield == Options.Structural then
			index = i
			break
		end
	end
	if index > 0 then
		var value = s.tree.expression.expressions[i].value
		assert(value and (value==true or value==false),
			string.format("'%s' option for a random choice must be a boolean constant", Options.Structural))
		return value
	else
		-- All variables are structural by default
		return true
	end
end



-- Scalar-valued random choices can be subject to lower and upper bounds
local BoundState = {
	None = 0,
	Lower = 1,
	Upper = 2,
	LowerUpper = 3
}
-- The bounding state of a random choice is determined by arguments
--    passed in the optional argument struct.
local function getBoundState(opts)
	if not opts then
		return BoundState.None
	end
	local haslo = structHasMember(opts, Options.LowerBound)
	local hashi = structHasMember(opts, Options.UpperBound)
	if haslo and hashi then
		return BoundState.LowerUpper
	elseif haslo then
		return BoundState.Lower
	elseif hashi then
		return BoundState.Upper
	else
		return BoundState.None
	end
end
-- We implement bounding via variable transformations
local logistic = macro(function(x)
	return `1.0 / (1.0 + ad.math.exp(-x))
end)
local invlogistic = macro(function(y)
	return `-ad.math.log(1.0/y - 1.0)
end)
local function getBoundingTransforms(boundState)
	local forward = nil
	local inverse = nil
	local priorincr = nil
	if boundState == BoundState.None then
		forward = function(x, obj) return x end
		inverse = function(y, obj) return y end
		priorincr = function(x, obj) return `0.0 end
	elseif boundState == BoundState.Lower then
		forward = function(x, obj) return `tmath.exp(x) + obj.lo end
		inverse = function(y, obj)
			local z = `tmath.fmax(y, obj.lo + 1e-15)
			return `tmath.log(z - obj.lo)
		end
		priorincr = function(x, obj) return x end
	elseif boundState == BoundState.Upper then
		forward = function(x, obj) return `obj.hi - tmath.exp(x) end
		inverse = function(y, obj)
			local z = `tmath.fmin(y, obj.hi - 1e-15)
			return `tmath.log(obj.hi - z)
		end
		priorincr = function(x, obj) return x end
	elseif boundState == BoundState.LowerUpper then
		foward = function(x, obj)
			return quote
				var logit = logistic(x)
				if x > [-math.huge] and logit == 0.0 then logit = 1e-15 end
				if x < [math.huge] and logit == 1.0 then logit = [1.0 - 1e-15] end
				var y = obj.lo + (obj.hi - obj.lo) * logit
			in
				y
			end
		end
		inverse = function(y, obj)
			local z = `tmath.fmax(tmath.fmin(y, obj.hi - 1e-15), obj.lo + 1e-15)
			local t = `(z - obj.lo) / (obj.hi - obj.lo)
			return `invlogistic(t)
		end
		priorincr = function(x, obj)
			return `tmath.log(obj.hi - obj.lo) - x - 2.0*tmath.log(1.0 + tmath.exp(-x))
		end
	else
		error("getBoundingTransforms - Unknown bounding state")
	end
	return forward, inverse, priorincr
end



function qs.makeRandomChoice(sampleAndLogprob, propose)

	-- The RandomChoice struct records the parameters and value of the random choice
	--    in the execution trace.
	local RandomChoice = S.memoize(function(real, isStructural, boundState)

		local sl = sampleAndLogprob(real)
		propose = propose or makeDefaultProposal(sl)
		local fwd, inv, lpincr = getBoundingTransforms(boundState)

		local function paramField(i) return string.format("param%d", i) end

		-- Declare struct type and add members for value, params, and bounds (if needed)
		local struct RandomChoiceT(S.Object)
		{
			logprob: real
		}
		local ValueType = sl.sample:gettype().returntype
		local ParamTypes = sl.sample:gettype().parameters
		-- Parameters that are pointer-to-struct are stored as struct value types
		local StoredParamTypes = ParamTypes:map(
			function(pt) if pt:ispointertostruct() then return pt.type else return pt end)
		RandomChoiceT.ValueType = ValueType
		RandomChoiceT.isStructural = isStructural
		RandomChoiceT.entries:insert({field="value", type=ValueType})
		for i,spt in ipairs(ParamTypes) do
			RandomChoiceT.entries:insert({field=paramField(i), type=spt})
		end
		local hasLowerBound = (boundState == BoundState.Lower or boundState == BoundState.LowerUpper)
		if hasLowerBound then
			RandomChoiceT.entries:insert({field=Options.LowerBound, type=real})
		end
		local hasUpperBound = (boundState == BoundState.Upper or boundState == BoundState.LowerUpper)
		if hasUpperBound then
			RandomChoiceT.entries:insert({field=Options.UpperBound, type=real})
		end

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

		-- Constructor 1: Has an initial value
		local paramSyms = ParamTypes:map(function(pt) return symbol(pt) end)
		local initValSym = symbol(util.isPOD(ValueType) and ValueType or &ValueType)
		local loHiSyms = terralib.newlist()
		if hasLowerBound then loHiSyms:insert(symbol(real)) end
		if hasUpperBound then loHiSyms:insert(symbol(real)) end
		terra RandomChoiceT:__init([paramSyms], initValSym, [loHiSyms]) : {}
			escape
				for i=1,#ParamTypes do
					emit quote S.copy(self.[paramField(i)], [paramSyms[i]]) end
				end
				if hasLowerBound then
					emit quote self.[Options.LowerBound] = [loHiSyms[1]] end
				end
				if hasUpperBound then
					emit quote self.[Options.UpperBound] = [loHiSyms[2]] end
				end
			end
			S.copy(self.value, inv(initValSym, self))
			self:rescore()
		end

		-- Constructor 2: Has no initial value
		paramSyms = ParamTypes:map(function(pt) return symbol(pt) end)
		loHiSyms = terralib.newlist()
		if hasLowerBound then loHiSyms:insert(symbol(real)) end
		if hasUpperBound then loHiSyms:insert(symbol(real)) end
		terra RandomChoiceT:__init([paramSyms], [loHiSyms]) : {}
			-- Draw a sample, then call the other constructor
			var sampledval = sl.sample([paramSyms])
			RandomChoiceT:__init(sampledval, [paramSyms], [loHiSyms])
		end

		-- Copy constructor
		terra RandomChoiceT:__copy(other: &RandomChoiceT) : {}
			S.copy(self.value, &other.value)
			escape
				for i=1,#ParamTypes do
					emit quote S.copy(self.[paramFields(i)], &other.[paramFields(i)]) end
				end
				if hasLowerBound then
					emit quote self.[Options.LowerBound] = other.[Options.LowerBound] end
				end
				if hasUpperBound then
					emit quote self.[Options.UpperBound] = other.[Options.UpperBound] end
				end
			end
		end

		-- Update for a new run of the program, checking for changes
		paramSyms = ParamTypes:map(function(pt) return symbol(pt) end)
		loHiSyms = terralib.newlist()
		if hasLowerBound then loHiSyms:insert(symbol(real)) end
		if hasUpperBound then loHiSyms:insert(symbol(real)) end
		terra RandomChoiceT:update([paramSyms], [loHiSyms]) : {}
			var hasChanges = false
			escape
				-- If real is not a primitive float/double type, then we must
				--    always update everything (otherwise memory bugs...)
				if not real:isfloat() then
					hasChanges = true
					for i=1,#ParamTypes do
						emit quote
							S.rundestructor(self.[paramField(i)])
							S.copy(self.[paramField(i)], [paramSyms[i]])
						end
					end
					if hasLowerBound then
						emit quote self.[Options.LowerBound] = [loHiSyms[1]] end
					end
					if hasUpperBound then
						emit quote self.[Options.UpperBound] = [loHiSyms[2]] end
					end
				-- Otherwise, we only need to copy/rescore if we found something
				--    that's changed since last time.
				else
					for i=1,#ParamTypes do
						local p = `[paramSyms[i]]
						-- __eq operator takes value types
						if ParamTypes[i]:ispointertostruct() then p = `@p end
						emit quote
							if not (self.[paramField(i)] == p) then
								hasChanges = true
								S.rundestructor(self.[paramField(i)])
								S.copy(self.[paramField(i)], [paramSyms[i]])
							end
						end
					end
					if hasLowerBound then
						emit quote
							if not (self.[Options.LowerBound] == [loHiSyms[1]]) then
								hasChanges = true
								self.[Options.LowerBound] = [loHiSyms[1]]
							end
						end
					end
					if hasUpperBound then
						emit quote
							if not (self.[Options.UpperBound] == [loHiSyms[2]]) then
								hasChanges = true
								self.[Options.UpperBound] = [loHiSyms[2]]
							end
						end
					end
				end
			end
			if hasChanges
				self:rescore()
			end
		end

		-- Rescore by recomputing prior logprob
		terra RandomChoiceT:rescore() : {}
			var val = self:getValue()
			self.logprob = sl.logprob(escape
				if sl.logprob:gettype().parameters[1] == &ValueType then
					emit `&val
				else
					emit `val
				end
			end, [paramArgList(self)]) + lpincr(val, self)
		end

		-- Propose new value, return fwd/rvs proposal probabilities
		terra RandomChoiceT:propose() : {real, real}
			var fwdlp : real
			var rvslp : real
			S.rundestructor(self.value)
			var val = self:getValue()
			self.value, fwdlp, rvslp = propose(escape
				if propose:gettype().parameters[1] == &ValueType then
					emit `&val
				else
					emit `val
				end
			end, [paramArgList(self)])
			self.value = inv(self.value, self)
			self:rescore()
			return fwdlp, rvslp
		end

		-- Get the (transformed) stored value of this random choice
		RandomChoiceT.methods.getValue() = macro(function()
			return `fwd(self.value, self)
		end)


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
		local sl = sampleAndLogprob(qs.real)
		local n = getNumParams(sl)
		local N = #args
		assert(N == n or N == n+1, "erp.observe - Too many parameters")
		local opts = nil
		if N == n+1 then
			local opts = args[N]
			args[N] = nil
		end
		local bs = getBoundState(opts)
		local fwd, inv, lpincr = getBoundingTransforms(bs)
		local isStructural = getStructuralOption(opts)
		local RandomChoiceT = RandomChoice(qs.real, isStructural, bs)
		local ValType = sl.sample:gettype().returntype
		---------------------
		-- Options to be passed to the RandomChoice constructor or update method
		-- (initial value, structural/nonstructural, lower/upper bounds)
		---------------------
		local ctoropts = terralib.newlist()
		local updateopts = terralib.newlist()
		if structHasMember(opts, Options.InitialVal) then
			ctoropts:insert((`opts.[Options.InitialVal]))
		end
		if structHasMember(opts, Options.LowerBound) then
			ctoropts:insert((`opts.[Options.LowerBound]))
			updateopts:insert((`opts.[Options.LowerBound]))
		end
		if structHasMember(opts, Options.UpperBound) then
			ctoropts:insert((`opts.[Options.UpperBound]))
			updateopts:insert((`opts.[UpperBound]))
		end
		---------------------
		return quote
			var val : ValType
			if qs.isRecordingTrace then
				-- Look up value in the currently-executing trace
				-- IMPORTANT: This should record that this program is using this RandomChoice type.
				--            Non-POD values should be returned by pointer
				var lookupval = [trace.lookupRandomChoice(RandomChoiceT, args, ctoropts, updateopts)]
				-- Copy the value
				S.copy(val, lookupval)
			else
				-- Just draw a sample (respect bounds)
				val = fwd(inv(sl.sample([args]), opts), opts)
			end
			-- defer destruct if ValType has a destructor
			escape
				if ValType:isstruct() and ValType:getmethod("destruct") then
					emit quote defer val:destruct() end
				end
			end
		in
			-- Return pointer-to-struct, in keeping with salloc() convention,
			--    if ValType is non-POD
			[util.isPOD(ValType) and (`val) or (`&val)]
		end
	end)

	-- This macro facilitates models where we directly observe the values
	--    of some random choices, instead of sampling them
	erp.observe = macro(function(value, ...)
		local args = {...}
		local sl = sampleAndLogprob(qs.real)
		local n = getNumParams(sl)
		local N = #args
		assert(N == n or N == n+1, "erp.observe - Too many parameters")
		local opts = nil
		if N == n+1 then
			local opts = args[N]
			args[N] = nil
		end
		local fwd, inv, lpincr = getBoundingTransforms(getBoundState(opts))
		return quote
			qs.factor(sl.logprob(value, [args]) + lpincr(value, opts))
		end
	end)

end





