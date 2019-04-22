--go@ luajit -jp=z *
setfenv(1, require'layerlib_types')
require'layerlib_border'
require'layerlib_background'
require'layerlib_shadow'
require'layerlib_text'
require'layerlib_draw'
require'layerlib_layout'

--init/free ------------------------------------------------------------------

terra Layer:init(manager: &LayerManager)

	fill(self)

	self.manager = manager
	self.children:init()

	self.snap_x = true
	self.snap_y = true

	self.visible = true
	self.opacity = 1

	self.layout_solver = &null_layout
end

terra Layer:free(): {}
	self.children:free()

	self:free_border()
	self:free_background()
	self:free_shadow()
	self:free_text()

	self.layout_type = LAYOUT_NULL

	if self.parent ~= nil then
		self.parent.children:remove(self)
	else
		self.manager.layers:release(self.manager.layers:index(self))
	end
end

--layer manager --------------------------------------------------------------

terra LayerManager:init()
	self.layers:init()
	self.text_renderer:init()
	self.fonts:init()

	self.borders:init()
	self.borders:alloc()
	self.borders:at(0):init()

	self.backgrounds:init()
	self.backgrounds:alloc()
	self.backgrounds:at(0):init()

	self.shadows:init()
	self.shadows:alloc()
	self.shadows:at(0):init()

	self.texts:init()
	self.texts:alloc()
	self.texts:at(0):init(&self.text_renderer)

	self.flexs:init()
	self.flexs:alloc()
	self.flexs:at(0):init()

	self.grids:init()
	self.grids:alloc()
	self.grids:at(0):init()

	self.grid_occupied:init()

	self.default_text_span:init()
end

terra LayerManager:free()
	self.layers        :free()
	self.text_renderer :free()
	self.borders       :free()
	self.backgrounds   :free()
	self.shadows       :free()
	self.texts         :free()
	self.fonts         :free()
	self.flexs         :free()
	self.grids         :free()
	self.grid_occupied :free()
	self.default_text_span:free()
end

terra LayerManager:layer()
	var i = self.layers:alloc()
	var e = self.layers:at(i)
	e:init(self)
	return e
end

terra layer_manager()
	var man = alloc(LayerManager); man:init()
	return man
end

return layerlib
