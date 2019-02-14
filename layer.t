
setfenv(1, require'low')
import'oops'

local class layer nocompile end

--field layer.doh: int = 1
field layer.children: arr(layer)
--field layer.parent: &layer = nil

--[[
method layer:remove_layer(e: &layer): bool
	var i = self.children:indexof(e)
	if i == -1 then return false end
	self.children:remove(i)
	return true
end

method layer:add_layer(e: &layer)
	if e.parent ~= nil then
		e.parent:remove_layer(e)
	end
	e.parent = self
end

]]
layer:compile()

layer.metamethods.__call = terra(self: &layer, i: int): &layer
	return self.children:at(i)
end

if not ... then

terra test()
	var e = layer(nil)
	var e1 = layer(nil)
	--e:add_layer(e1)
end
test()

end

return layer
