
setfenv(1, require'low')
local view = require'arrayview'

terra test()
	var s = 'Hello World!'
	var v: view(int8)
	v:fromcstring(s)
	print(v.len)
	print(v:at(11))
	print(v:at(12, nil))
	print(v(11))
	print(v(12, -1))
	for i,v in v do
		fprintf(stdout(), '%d ', @v)
	end
	print()
	var v2 = v:view(0, v.len)
	for i,v in v2:backwards() do
		fprintf(stdout(), '%d ', @v)
	end
	print()
	assert(v == v2)
	v2.len = v2.len - 1
	assert(v ~= v2)
	assert(v > v2)
	assert(v >= v2)
	assert(not (v < v2))
	assert(not (v <= v2))
end
test()
