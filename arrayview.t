--[[

	Array view type for Terra.
	Written by Cosmin Apreutesei. Public domain.

	An array view maps the idea of an array onto any memory location.

	v: arrayview{T=,[cmp=],[size_t=int32],[C=require'low']}
	v = arrayview(T,[cmp],[size_t=int32])
	v = arrayview(T,p,len,[cmp],[size_t=int32])
	v:fromcstring(cstring) -> self
	v.elements
	v.len
	v1 <|<=|==|>=|>|~= v2

	v:range(i, j) -> start, len
	v:view(i, j) -> v

	v:at(i[,default]) -> &v|default
	v(i[,default]) -> v|default
	v:set(i, v)
	for i,v in v[:backwards()] do ... end

	v:copy([v|&v]) -> v|&v
	v:update(i, v|&v,len) -> ok?

	v:sort([cmp: {&T, &T} -> int32])
	v:sort_desc()
	v:find(v) -> i
	v:count(v) -> n
	v:binsearch(v, cmp: {&T, &T} -> bool) -> i
	v:binsearch(v, a.lt|a.lte|a.gt|a.gte) -> i
	v:binsearch_macro(v, cmp(t, i, v) -> bool) -> i

	v:reverse()
	v:call(method, args...)

	v:indexof(&v[,default]) -> i|default
	v:next([default]) -> &v|default
	v:prev([default]) -> &v|default

]]

if not ... then require'arrayview_test'; return end

local either = macro(function(v, a, b) return `v == a or v == b end)

local function view_type(T, cmp, size_t, C)

	setfenv(1, C)

	local struct view {
		elements: &T;
		len: size_t; --number of valid elements
	}

	--debugging

	function view.metamethods.__typename(self)
		return 'arrayview('..tostring(T)..')'
	end

	function view.metamethods.__tostring(self, format_arg, fmt, args, freelist)
		add(fmt, '%s[%d]')
		add(args, tostring(T))
		add(args, `self.len)
	end

	--storage

	--initialize with a null-terminated string
	terra view:fromcstring(s: &int8)
		self.len = strnlen(s, [size_t:max()]-1)
		self.elements = s
		return self
	end

	--bounds-checked access

	view.methods.at = overload('at', {})
	view.methods.at:adddefinition(terra(self: &view, i: size_t): &T
		if i < 0 then i = self.len + i end
		assert(i >= 0 and i < self.len)
		return &self.elements[i]
	end)
	view.methods.at:adddefinition(terra(self: &view, i: size_t, default: &T): &T
		if i < 0 then i = self.len + i end
		return iif(i >= 0 and i < self.len, &self.elements[i], default)
	end)

	view.metamethods.__apply = overload('get', {})
	view.metamethods.__apply:adddefinition(terra(self: &view, i: size_t): T
		if i < 0 then i = self.len + i end
		assert(i >= 0 and i < self.len)
		return self.elements[i]
	end)
	view.metamethods.__apply:adddefinition(terra(self: &view, i: size_t, default: T): T
		if i < 0 then i = self.len + i end
		return iif(i >= 0 and i < self.len, self.elements[i], default)
	end)

	terra view:set(i: size_t, val: T)
		if i < 0 then i = self.len + i end
		assert(i >= 0 and i < self.len)
		self.elements[i] = val
	end

	--iteration

	view.metamethods.__for = function(self, body)
		return quote
			for i = 0, self.len do
				[ body(i, `&self.elements[i]) ]
			end
		end
	end

	local struct reverse_iter { view: view; }
	reverse_iter.metamethods.__for = function(self, body)
		return quote
			for i = self.view.len-1, -1, -1 do
				[ body(i, `&self.view.elements[i]) ]
			end
		end
	end
	terra view:backwards() return reverse_iter {@self} end

	--sub-views

	--NOTE: j is not the last position, but one position after that!
	terra view:range(i: size_t, j: size_t)
		if i < 0 then i = self.len + i end
		if j < 0 then j = self.len + j end
		assert(i >= 0)
		j = min(max(i, j), self.len)
		return i, j-i
	end

	view.methods.view = overload('view', {})
	view.methods.view:adddefinition(terra(self: &view, i: size_t, j: size_t)
		var start, len = self:range(i, j)
		return view {elements = self.elements + start, len = len}
	end)
	view.methods.view:adddefinition(terra(self: &view, i: size_t)
		return self:view(i, self.len)
	end)

	--copy out

	view.methods.copy = overload('copy', {})
	view.methods.copy:adddefinition(terra(self: &view, dst: &T)
		memmove(dst, self.elements, self.len)
		return dst
	end)
	view.methods.copy:adddefinition(terra(self: &view, dst: view)
		memmove(dst.elements, self.elements, min(dst.len, self.len))
		return dst
	end)

	--copy in

	view.methods.update = overload('update', {})
	view.methods.update:adddefinition(terra(self: &view, src: &T, len: size_t)
		if len <= 0 then return end
		memmove(&self.elements, src, sizeof(T) * min(len, self.len))
	end)
	view.methods.update:adddefinition(terra(self: &view, v: view)
		return self:update(v.elements, v.len)
	end)

	--comparing views for inequality

	--1. elements are comparable via cmp or __cmp metamethod.

	--2. elements are comparable via metamethods.
	local custom_op = not cmp and T:isaggregate()
		and T.metamethods.__eq and T.metamethods.__lt

	--3. elements are inherently comparable or comparable via metamethods.
	if not cmp and (custom_op or T:isarithmetic() or T:ispointer()) then

		cmp = terra(a: &T, b: &T): int32
			return iif(@a == @b, 0, iif(@a < @b, -1, 1))
		end

		--4. uint8 views can even be mem-compared directly.
		if not custom_op and T == uint8 then
			terra view:__cmp(v: &view)
				if v.len ~= self.len then
					return iif(self.len < v.len, -1, 1)
				end
				return memcmp(self.elements, v.elements, self.len)
			end
		end
	end

	--compare views based on cmp.
	if cmp and not view.metamethods.__cmp then
		view.metamethods.__cmp = terra(self: &view, v: &view)
			if v.len ~= self.len then
				return iif(self.len < v.len, -1, 1)
			end
			for i, val in self do
				var r = cmp(val, v:at(i))
				if r ~= 0 then
					return r
				end
			end
			return 0
		end
	end

	local __cmp = view.metamethods.__cmp
	if __cmp then
		view.metamethods.__lt = terra(self: &view, v: &view) return __cmp(self, v) == -1 end
		view.metamethods.__gt = terra(self: &view, v: &view) return __cmp(self, v) ==  1 end
		view.metamethods.__le = terra(self: &view, v: &view) return either(__cmp(self, v), -1, 0) end
		view.metamethods.__ge = terra(self: &view, v: &view) return either(__cmp(self, v),  1, 0) end
		view.metamethods.__eq = terra(self: &view, v: &view) return __cmp(self, v) == 0 end
	end

	--comparing views for equality

	if not view.metamethods.__eq then
		--floats might not be normalized, compare them individually.
		if (T:isaggregate() and T.metamethods.__eq) or T:isfloat() then
			view.metamethods.__eq = terra(self: &view, v: &view)
				if v.len ~= self.len then return false end
				for val in self do
					if not (val == v.elements[i]) then return false end
				end
				return true
			end
		else --use memcmp for everything else
			view.metamethods.__eq = terra(self: &view, v: &view)
				if v.len ~= self.len then return false end
				return memcmp(self.elements, v.elements, sizeof(T) * self.len) == 0
			end
		end
	end

	local __eq = view.metamethods.__eq
	if __eq then
		view.metamethods.__ne = terra(self: &view, v: &view) return not __eq(self, v) end
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

	view.methods.sort = overload('sort', {})
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

	local eq = cmp and macro(function(a, b) return `cmp(a, b) == 0 end)
	if not eq and T:isaggregate() and not T.metamethods.__eq then --use memcmp
		eq = macro(function(a, b) return `memcmp(a, b, sizeof(T)) == 0 end)
	end
	eq = eq or macro(function(a, b) return `@a == @b end)

	terra view:find(val: T)
		for i, v in self do
			if eq(v, &val) then
				return i
			end
		end
		return -1
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
		local props = addproperties(view)
		props.lt = macro(function() return lt  end)
		props.gt = macro(function() return gt  end)
		props.le = macro(function() return le end)
		props.ge = macro(function() return ge end)
	end

	view.methods.binsearch = overload('binsearch', {})
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

	local cmp_lt = macro(function(t, i, v) return `t[i] < v end)
	view.methods.binsearch_macro = macro(function(self, v, cmp)
		cmp = cmp or cmp_lt
		return `binsearch(v, self.elements, 0, self.len-1, cmp)
	end)

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
		local method = T.methods[method_name:asvalue()]
		local args = {...}
		return quote
			for i,v in self do
				method(v, [args])
			end
		end
	end)

	--pointer interface

	view.methods.indexof = overload('indexof', {})
	view.methods.indexof:adddefinition(terra(self: &view, pv: &T, default: size_t)
		var i = pv - self.elements
		return iif(i >= 0 and i < self.len, i, default)
	end)
	view.methods.indexof:adddefinition(terra(self: &view, pv: &T)
		var i = pv - self.elements
		assert(i >= 0 and i < self.len)
		return i
	end)

	view.methods.next = overload('next', {})
	view.methods.next:adddefinition(terra(self: &view, pv: &T, default: &T)
		var i = pv - self.elements
		return iif(i >= 0 and i < self.len-1, self.elements + i + 1, default)
	end)
	view.methods.next:adddefinition(terra(self: &view, pv: &T)
		var i = pv - self.elements
		assert(i >= 0 and i < self.len-1)
		return self.elements + i + 1
	end)

	view.methods.prev = overload('prev', {})
	view.methods.prev:adddefinition(terra(self: &view, pv: &T, default: &T)
		var i = pv - self.elements
		return iif(i > 0 and i < self.len, self.elements + i - 1, default)
	end)
	view.methods.prev:adddefinition(terra(self: &view, pv: &T)
		var i = pv - self.elements
		assert(i > 0 and i < self.len)
		return self.elements + i - 1
	end)

	return view
end
view_type = terralib.memoize(view_type)

local view_type = function(T, cmp, size_t, C)
	if terralib.type(T) == 'table' then
		T, cmp, size_t, C = T.T, T.cmp, T.size_t, T.C
	end
	assert(T)
	cmp = cmp or (T:isaggregate() and T.metamethods.__cmp)
	size_t = size_t or int32
	C = C or require'low'
	return view_type(T, cmp, size_t, C)
end

local view = macro(
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

return view
