--[[

	Array view type for Terra.
	Written by Cosmin Apreutesei. Public domain.

	An array view maps the idea of an array onto any memory location.

	local V = arrayview{T=,[cmp=],[size_t=int]} create a type from Lua
	local V = arrayview(T, [cmp=],[size_t=int]) create a type from Lua
	var v =   arrayview{T=,[cmp=],[size_t=int]} create a value from Terra
	var v =   arrayview(T, [cmp=],[size_t=int]) create a value from Terra
	var v =   arrayview(T, elements,len[ ,...]) create a value from Terra
	var v = V(nil)                              nil-cast for use in constant()
	var v = V{elements,len}                     field order is part of the API
	var v = V{elements=,len=}                   fields are part of the API

	var s = V(rawstring|'string constant')      cast from C string
	v:onrawstring(rawstring) -> v               init with C string
	v.elements, v.len                           fields are part of the API

	v:index(i[,default]) -> i|default           valid positive index
	v(i[,default]) -> T|default                 get element at index
	v:at(i[,default]) -> &T|default             get element address at index
	v:set(i,T)                                  set element at index
	for i,&T in v[:backwards()] do ... end      iterate elements

	v:range(i,j) -> start,len                   v:range(5, 5) -> 5, 0
	v:sub(i,j) -> v                             create a sub-view
	v:copy(&T) -> &T                            copy to buffer
	v:copy(&v) -> &v                            copy to view

	v:__cmp(&v) -> -1,0,1                       comparison function
	v:__eq(&v) -> equal?                        equality function
	v:__hash32([h0=0]) -> h                     32bit hash function
	v:__hash64([h0=0]) -> h                     64bit hash function

	v1 <|<=|==|>=|>|~= v2                       compare views

	cmp = {&T, &T} -> int32                     type of element comparison function
	v:sort([cmp])                               sort elements
	v:sort_desc()                               sort descending
	v:find(T[,default]) -> i|-1                 find element
	v:count(T) -> n                             element occurences
	v:binsearch(T, [cmp]) -> i                  binsearch (sort the view first!)
	v:binsearch(T, v.lt|v.lte|v.gt|v.gte) -> i  binsearch with built-in cmp

	v:reverse()                                 reverse order of elements
	v:call(method, args...)                     call method on each element

	v:index(&T[,default]) -> i|default          element index by address
	v:next(&T,[default]) -> &T|default          next element
	v:prev(&T,[default]) -> &T|default          previous element

]]

if not ... then require'arrayview_test'; return end

setfenv(1, require'low')

local either = macro(function(v, a, b) return `v == a or v == b end)

local function view_type(T, cmp, size_t)

	local struct view {
		elements: &T;
		len: size_t; --number of valid elements
	}

	view.empty = `view{elements = nil, len = 0}

	function view.metamethods.__cast(from, to, exp)
		if from == niltype then --makes [arrview(T)](nil) work in a constant()
			return view.empty
		elseif T == int8 and from == rawstring then
			return `view(nil):onrawstring(exp)
		else
			error'invalid cast'
		end
	end

	--debugging

	function view.metamethods.__typename(self)
		return 'arrayview('..tostring(T)..')'
	end

	function view.metamethods.__tostring(self, format_arg, fmt, args, freelist, indent)
		add(fmt, '%s[%d]<%llx>')
		add(args, tostring(T))
		add(args, `self.len)
		add(args, `self.elements)
	end

	--default method

	view.metamethods.__apply = macro(function(self, i, default)
		if default then return `self:get(i, default) else return `self:get(i) end
	end)

	--iteration

	view.metamethods.__for = function(self, body)
		return quote
			for i = 0, self.len do
				[ body(i, `&self.elements[i]) ]
			end
		end
	end

	local struct backwards_iter { view: &view; }
	backwards_iter.metamethods.__for = function(self, body)
		return quote
			for i = self.view.len-1, -1, -1 do
				[ body(i, `&self.view.elements[i]) ]
			end
		end
	end

	addmethods(view, function()

		--storage

		--initialize with a null-terminated string
		if &T == rawstring then
			terra view:onrawstring(s: rawstring)
				self.len = strnlen(s, [size_t:max()]-1)
				self.elements = s
				return self
			end
		end

		--bounds-checked access

		view.methods.index = overload'index'
		view.methods.index:adddefinition(terra(self: &view, i: size_t, default: size_t)
			if i < 0 then i = self.len + i end
			return iif(i >= 0 and i < self.len, i, default)
		end)
		view.methods.index:adddefinition(terra(self: &view, i: size_t)
			if i < 0 then i = self.len + i end
			assert(i >= 0 and i < self.len)
			return i
		end)

		view.methods.at = overload'at'
		view.methods.at:adddefinition(terra(self: &view, i: size_t): &T
			return &self.elements[self:index(i)]
		end)
		view.methods.at:adddefinition(terra(self: &view, i: size_t, default: &T): &T
			i = self:index(i, -1)
			return iif(i ~= -1, &self.elements[i], default)
		end)

		view.methods.get = overload'get'
		view.methods.get:adddefinition(terra(self: &view, i: size_t): T
			return self.elements[self:index(i)]
		end)
		view.methods.get:adddefinition(terra(self: &view, i: size_t, default: T): T
			i = self:index(i, -1)
			return iif(i ~= -1, self.elements[i], default)
		end)

		terra view:set(i: size_t, val: T)
			self.elements[self:index(i)] = val
		end

		--iteration

		terra view:backwards() return backwards_iter {self} end

		--sub-views

		--NOTE: j is not the last position, but one position after that.
		--This is for compatibility with __for.
		--NOTE: This makes 0 ambiguous with view.len.
		terra view:range(i: size_t, j: size_t)
			if i < 0 then i = self.len + i end
			if j < 0 then j = self.len + j end
			assert(i >= 0)
			j = min(max(i, j), self.len)
			return i, j-i
		end

		view.methods.sub = overload'sub'
		view.methods.sub:adddefinition(terra(self: &view, i: size_t, j: size_t)
			var start, len = self:range(i, j)
			return view {elements = self.elements + start, len = len}
		end)
		view.methods.sub:adddefinition(terra(self: &view, i: size_t)
			return self:sub(i, self.len)
		end)

		--copy out

		view.methods.copy = overload'copy'
		view.methods.copy:adddefinition(terra(self: &view, dst: &T)
			copy(dst, self.elements, self.len)
			return dst
		end)
		view.methods.copy:adddefinition(terra(self: &view, dst: &view)
			copy(dst.elements, self.elements, min(dst.len, self.len))
			return dst
		end)

		local user_cmp = cmp --keep the custom comparison function

		--comparing views for inequality

		--elements must be compared via comparison operators.
		local use_op = not cmp and T:isaggregate()
			and T.metamethods.__eq and T.metamethods.__lt

		--elements must or can be compared via comparison operators.
		if not cmp and (use_op or T:isarithmetic() or T:ispointer()) then

			cmp = terra(a: &T, b: &T): int32 --for sorting this view
				return iif(@a == @b, 0, iif(@a < @b, -1, 1))
			end

			--uint8 arrays can even be mem-compared directly.
			if not custom_op and T == uint8 then
				terra view:__cmp(v: &view) --for comparing views
					if v.len ~= self.len then
						return iif(self.len < v.len, -1, 1)
					end
					return memcmp(self.elements, v.elements, self.len)
				end
			end
		end

		--compare views by comparing elements individually.
		if cmp and not view.methods.__cmp then
			terra view:__cmp(v: &view)
				if v.len ~= self.len then
					return iif(self.len < v.len, -1, 1)
				end
				for i,val in self do
					var r = cmp(val, v:at(i))
					if r ~= 0 then
						return r
					end
				end
				return 0
			end
		end

		--make all inequality comparison operators work too.
		if view.methods.__cmp then
			view.metamethods.__lt = terra(self: &view, v: &view) return self:__cmp(v) == -1 end
			view.metamethods.__gt = terra(self: &view, v: &view) return self:__cmp(v) ==  1 end
			view.metamethods.__le = terra(self: &view, v: &view) return either(self:__cmp(v), -1, 0) end
			view.metamethods.__ge = terra(self: &view, v: &view) return either(self:__cmp(v),  1, 0) end
		end

		--comparing views for equality

		if user_cmp then

			--elements must be compared via custom comparison function.
			terra view:__eq(v: &view)
				return self:__cmp(v) == 0
			end

		else

			--elements must be compared via comparison operator.
			local use_op = T:isaggregate() and T.metamethods.__eq

			--elements must or can be compared via comparison operators.
			--floats need this since they may not be normalized.
			if not user_cmp and (use_op or T:isfloat()) then
				terra view:__eq(v: &view)
					if v.len ~= self.len then return false end
					for i,val in self do
						if @val ~= v.elements[i] then return false end
					end
					return true
				end
			else --use memcmp
				terra view:__eq(v: &view)
					if v.len ~= self.len then return false end
					return equal(self.elements, v.elements, self.len)
				end
			end

		end

		--make the == operator work too.
		view.metamethods.__eq = view.methods.__eq

		--make the ~= operator work too.
		if view.metamethods.__eq then
			view.metamethods.__ne = macro(function(self, other)
				return not (self == other)
			end)
		end

		--hashing using the default hash function

		if hash then
			view.methods.__hash32 = macro(function(self, d)
				return `hash(uint32, self.elements, self.len * sizeof(T), [d or 0])
			end)
			view.methods.__hash64 = macro(function(self, d)
				return `hash(uint64, self.elements, self.len * sizeof(T), [d or 0])
			end)
		end

		--memsize for caches and debugging

		terra view:__memsize(): size_t
			return sizeof(view) + sizeof(T) * self.len
		end

		--sorting

		view.methods.sort = overload'sort'
		view.methods.sort:adddefinition(terra(self: &view, cmp: {&T, &T} -> int32)
			qsort(self.elements, self.len, sizeof(T),
				[{&opaque, &opaque} -> int32](cmp))
			return self
		end)

		if cmp then
			view.methods.sort:adddefinition(terra(self: &view)
				return self:sort(cmp)
			end)
			local terra cmp_desc(a: &T, b: &T): int32
				return -cmp(a, b)
			end
			terra view:sort_desc() return self:sort(cmp_desc) end
		end

		--searching

		local eq = user_cmp and macro(function(a, b) return `user_cmp(a, b) == 0 end)
		if not eq and T:isaggregate() and not T.metamethods.__eq then --use memcmp
			eq = macro(function(a, b) return `memcmp(a, b, sizeof(T)) == 0 end)
		end
		eq = eq or macro(function(a, b) return `@a == @b end)

		terra view:find(val: T, default: size_t)
			for i, v in self do
				if eq(v, &val) then
					return i
				end
			end
			return default
		end

		terra view:count(val: T)
			var n: size_t = 0
			for i, v in self do
				if eq(v, &val) then
					n = n + 1
				end
			end
			return n
		end

		--binary search for an insert position that keeps the array sorted.

		local lt, gt, lte, gte
		if cmp then
			lt = terra(a: &T, b: &T) return cmp(a, b) == -1 end
			gt = terra(a: &T, b: &T) return cmp(a, b) ==  1 end
			le = terra(a: &T, b: &T) return either(cmp(a, b), -1, 0) end
			ge = terra(a: &T, b: &T) return either(cmp(a, b),  1, 0) end
		elseif not T:isaggregate() then
			lt = terra(a: &T, b: &T) return @a <  @b end
			gt = terra(a: &T, b: &T) return @a >  @b end
			le = terra(a: &T, b: &T) return @a <= @b end
			ge = terra(a: &T, b: &T) return @a >= @b end
		end
		--expose comparators as virtual fields of the view.
		if lt then
			addproperties(view)
			view.properties.lt = lt
			view.properties.gt = gt
			view.properties.le = le
			view.properties.ge = ge
		end

		view.methods.binsearch = overload'binsearch'
		view.methods.binsearch:adddefinition(
		terra(self: &view, v: T, cmp: {&T, &T} -> bool): size_t
			var lo = [size_t](0)
			var hi = self.len-1
			var i = hi + 1
			while true do
				if lo < hi then
					var mid: int = lo + (hi - lo) / 2
					if cmp(&self.elements[mid], &v) then
						lo = mid + 1
					else
						hi = mid
					end
				elseif lo == hi and not cmp(&self.elements[lo], &v) then
					return lo
				else
					return i
				end
			end
		end)
		if lt then
			view.methods.binsearch:adddefinition(terra(self: &view, v: T): size_t
				return self:binsearch(v, lt)
			end)
		end

		--reversing the order of elements

		terra view:reverse()
			var j = self.len-1
			for k = 0, (j+1)/2 do
				var tmp = self.elements[k]
				self.elements[k] = self.elements[j-k]
				self.elements[j-k] = tmp
			end
			return self
		end

		--calling methods on the elements

		view.methods.call = macro(function(self, method_name, ...)
			local method = T:getmethod(method_name:asvalue())
			local args = {...}
			return quote
				for i,v in self do
					method(v, [args])
				end
			end
		end)

		--pointer interface

		view.methods.index:adddefinition(terra(self: &view, pv: &T, default: size_t)
			return self:index(pv - self.elements, default)
		end)
		view.methods.index:adddefinition(terra(self: &view, pv: &T)
			return self:index(pv - self.elements)
		end)

		view.methods.next = overload'next'
		view.methods.next:adddefinition(terra(self: &view, pv: &T, default: &T)
			var i = pv - self.elements
			return iif(i >= 0 and i < self.len-1, self.elements + i + 1, default)
		end)
		view.methods.next:adddefinition(terra(self: &view, pv: &T)
			var i = pv - self.elements
			assert(i >= 0 and i < self.len-1)
			return self.elements + i + 1
		end)

		view.methods.prev = overload'prev'
		view.methods.prev:adddefinition(terra(self: &view, pv: &T, default: &T)
			var i = pv - self.elements
			return iif(i > 0 and i < self.len, self.elements + i - 1, default)
		end)
		view.methods.prev:adddefinition(terra(self: &view, pv: &T)
			var i = pv - self.elements
			assert(i > 0 and i < self.len)
			return self.elements + i - 1
		end)

	end) --addmethods

	return view
end
view_type = memoize(view_type)

local view_type = function(T, cmp, size_t)
	if terralib.type(T) == 'table' then
		T, cmp, size_t = T.T, T.cmp, T.size_t
	end
	assert(T)
	cmp = cmp or (T:isaggregate() and (T.metamethods.__cmp or T:getmethod'__cmp'))
	size_t = size_t or int
	return view_type(T, cmp, size_t)
end

return macro(
	--calling it from Terra returns a new view.
	function(arg1, ...)
		local T, lval, len, cmp, size_t
		if arg1 and arg1:islvalue() then --wrap raw pointer: arrayview(&v, len, cmp, size_t)
			lval, len, cmp, size_t = arg1, ...
			T = lval:gettype()
			assert(T:ispointer())
			T = T.type
		else --create new view: arrayview(T, cmp, size_t)
			T, cmp, size_t = arg1, ...
			T = T and T:astype()
		end
		size_t = size_t and size_t:astype()
		local view = view_type(T, cmp, size_t)
		if lval then
			return quote var v: view; v.elements = lval; v.len = len in v end
		else
			return `view{}
		end
	end,
	--calling it from Lua or from an escape or in a type declaration returns
	--just the type, and you can also pass a custom C namespace.
	view_type
)
