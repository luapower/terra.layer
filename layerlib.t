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

terra Layer:free()
	self.children:free()

	self:free_border()
	self:free_background()
	self:free_shadow()
	self:free_text()

	self.layout_type = LAYOUT_NULL
end

terra LayerManager:free_layer(layer: &Layer)
	if layer.parent ~= nil then
		layer.parent.children:remove(layer)
	else
		self.layers:release(self.layers:index(layer))
	end
end

--layer manager --------------------------------------------------------------

terra LayerManager:init()
	self.layers:init()
	self.text_renderer:init()

	self.transforms:init()
	self.transforms:alloc()
	self.transforms:at(0):init()

	self.borders:init()
	self.borders:alloc()
	self.borders:at(0):init()

	self.backgrounds:init(self)
	self.backgrounds:alloc()
	self.backgrounds:at(0):init()

	self.shadows:init()
	self.shadows:alloc()
	self.shadows:at(0):init(nil)

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

	self.layers:free()

	--check that layers cleaned up after themselves.
	assert(self.transforms    .items.len == self.transforms .freeindices.len + 1)
	assert(self.borders       .items.len == self.borders    .freeindices.len + 1)
	assert(self.backgrounds   .items.len == self.backgrounds.freeindices.len + 1)
	assert(self.shadows       .items.len == self.shadows    .freeindices.len + 1)
	assert(self.texts         .items.len == self.texts      .freeindices.len + 1)
	assert(self.flexs         .items.len == self.flexs      .freeindices.len + 1)
	assert(self.grids         .items.len == self.grids      .freeindices.len + 1)

	--free the defaults too...
	self.transforms:free()
	self.borders:free()
	self.backgrounds:free()
	self.shadows:free()
	self.texts:free()
	self.flexs:free()
	self.grids:free()

	self.text_renderer:free()
	self.grid_occupied:free()
	self.default_text_span:free()
end

terra LayerManager:layer()
	var i = self.layers:alloc()
	var e = self.layers:at(i)
	e:init(self)
	return e
end

terra LayerManager:get_font_size_resolution       (): num return self.text_renderer.font_size_resolution end
terra LayerManager:get_subpixel_x_resolution      (): num return self.text_renderer.subpixel_x_resolution end
terra LayerManager:get_word_subpixel_x_resolution (): num return self.text_renderer.word_subpixel_x_resolution end
terra LayerManager:get_glyph_cache_size           () return self.text_renderer.glyphs.max_size end
terra LayerManager:get_glyph_run_cache_size       () return self.text_renderer.glyph_runs.max_size end

terra LayerManager:set_font_size_resolution       (v: num) self.text_renderer.font_size_resolution = v end
terra LayerManager:set_subpixel_x_resolution      (v: num) self.text_renderer.subpixel_x_resolution = v end
terra LayerManager:set_word_subpixel_x_resolution (v: num) self.text_renderer.word_subpixel_x_resolution = v end
terra LayerManager:set_glyph_cache_size           (v: int) self.text_renderer.glyphs.max_size = v end
terra LayerManager:set_glyph_run_cache_size       (v: int) self.text_renderer.glyph_runs.max_size = v end

return layerlib
