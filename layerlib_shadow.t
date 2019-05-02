
--shadow geometry and drawing
setfenv(1, require'layerlib_types')
require'layerlib_border'
require'layerlib_background'

terra Layer:shadow_visible()
	return self.shadow.blur > 0 or self.shadow.x ~= 0.0 or self.shadow.y ~= 0.0
end

terra Layer:shadow_spread()
	return self.shadow.passes * self.shadow.blur
end

terra Layer:shadow_bitmap_rect()
	var spread = self:shadow_spread()
	if self:border_visible() then
		return self:border_rect(1, spread)
	else
		return self:background_rect(spread)
	end
end

terra Layer:shadow_round_rect()
	var size = self:shadow_spread()
	if self:border_visible() then
		return self:border_round_rect(1, size)
	else
		return self:background_round_rect(size)
	end
end

terra Layer:shadow_path(cr: &cairo_t)
	if self:border_visible() then
		self:border_path(cr, 1, 0)
	else
		self:background_path(cr, 0)
	end
end

terra Layer:repaint_shadow(bmp: &Bitmap)
	var sr = bmp:surface(); defer sr:free()
	var cr = sr:context(); defer cr:free()
	cr:operator(CAIRO_OPERATOR_SOURCE)
	cr:rgba(0, 0, 0, 0)
	cr:paint()
	cr:translate(-self.shadow._state.x, -self.shadow._state.y)
	self:shadow_path(cr)
	cr:rgba(0, 0, 0, 1)
	cr:fill()
end

terra Layer:draw_shadow(cr: &cairo_t)
	if not self:shadow_visible() then return end
	var bx, by, bw, bh = self:shadow_bitmap_rect()
	self.shadow._state.x = bx
	self.shadow._state.y = by
	if self.shadow._state.blurred_surface == nil then
		var bmp = self.shadow._state.blur:blur(
			bw, bh, self.shadow.blur, self.shadow.passes)
		self.shadow._state.blurred_surface = bmp:surface()
	end
	var sx = bx + self.shadow.x
	var sy = by + self.shadow.y
	cr:translate(sx, sy)
	cr:rgba(self.shadow.color)
	cr:mask(self.shadow._state.blurred_surface, 0, 0)
	cr:translate(-sx, -sy)
end

terra Shadow:init(layer: &Layer)
	fill(self)
	self._state.blur:init(
		BITMAP_G8,
		[BlurRepaintFunc]([Layer:getmethod'repaint_shadow']),
		layer)
end

terra Shadow:invalidate()
	if self._state.blurred_surface ~= nil then
		self._state.blurred_surface:free()
		self._state.blurred_surface = nil
	end
end

terra Shadow:free()
	self:invalidate()
	self._state.blur:free()
end
