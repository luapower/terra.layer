
--background geometry & drawing

setfenv(1, require'layerlib_types')
require'layerlib_border'

terra BackgroundPattern:init()
	fill(self)
	self.extend = BACKGROUND_EXTEND_REPEAT
end

terra Background:init()
	fill(self)
	self.hittable = true
	self.operator = OPERATOR_OVER
	self.clip_border_offset = 1
end

terra BackgroundPattern:free(type: enum, man: &LayerManager)
	self.pattern:free()
	self.pattern = nil
	if type == BACKGROUND_TYPE_IMAGE then
		self.bitmap:free()
	else
		self.gradient:free()
	end
	if self.transform_id ~= 0 then
		man.transforms:release(self.transform_id)
		self.transform_id = 0
	end
end

terra Background:free(man: &LayerManager)
	if self.type > BACKGROUND_TYPE_COLOR then
		self.pattern:free(self.type, man)
	end
end

terra Layer:background_visible()
	return self.background.type ~= BACKGROUND_TYPE_NONE
end

terra Layer:background_rect(size_offset: num)
	return self:border_rect(self.background.clip_border_offset, size_offset)
end

terra Layer:background_round_rect(size_offset: num)
	return self:border_round_rect(self.background.clip_border_offset, size_offset)
end

terra Layer:background_path(cr: &cairo_t, size_offset: num)
	self:border_path(cr, self.background.clip_border_offset, size_offset)
end

terra Background:pattern()
	var p = &self.pattern
	if p.pattern == nil then
		if self.type == BACKGROUND_TYPE_LINEAR_GRADIENT
			or self.type == BACKGROUND_TYPE_RADIAL_GRADIENT
		then
			var g = p.gradient
			if self.type == BACKGROUND_TYPE_LINEAR_GRADIENT then
				var c = g.points
				p.pattern = cairo_pattern_create_linear(c.x1, c.y1, c.x2, c.y2)
			else
				var c = g.circles
				p.pattern = cairo_pattern_create_radial(c.cx1, c.cy1, c.r1, c.cx2, c.cy2, c.r2)
			end
			for _,c in g.color_stops do
				p.pattern:add_color_stop_rgba(c.offset, c.color)
			end
		elseif self.type == BACKGROUND_TYPE_IMAGE then
			p.pattern = cairo_pattern_create_for_surface(p.bitmap:surface())
		end
	end
	return p.pattern
end

terra Layer:paint_background(b: &Background, cr: &cairo_t)
	cr:operator(b.operator)
	if b.type == BACKGROUND_TYPE_COLOR then
		cr:rgba(b.color)
		cr:paint()
		return
	end
	var m: matrix; m:init()
	m:translate(b.pattern.x, b.pattern.y)
	self.manager.transforms:at(b.pattern.transform_id):apply(&m)
	m:invert()
	var patt = b:pattern()
	patt:matrix(&m)
	patt:extend(b.pattern.extend)
	cr:source(patt)
	cr:paint()
	cr:rgb(0, 0, 0) --release source
end
