
--text geometry & drawing

setfenv(1, require'layerlib_types')

terra Text:init(r: &tr.Renderer)
	fill(self)
	self.layout:init(r)
	self.layout.maxlen = 4096
	self.align_x = ALIGN_CENTER
	self.align_y = ALIGN_CENTER
	self.shaped = true
	self.wrapped = true
	self.caret_width = 1
	self.caret_color = color {0xffffffff}
	self.selectable = true
end

terra Text:free()
	self.layout:free()
end

terra Layer:text_visible()
	return self.text.layout.spans.len > 0
		and self.text.layout.spans:at(0).font_id ~= -1
		and self.text.layout.spans:at(0).font_size > 0
		and self.text.layout.text.len > 0
end

terra Layer:unshape()
	self.text.shaped = false
	self.text.wrapped = false
end

terra Layer:unwrap()
	self.text.wrapped = false
end

terra Layer:sync_text_shape()
	if not self:text_visible() then return false end
	if self.text.shaped then return true end
	self.text.layout:shape()
	self.text.shaped = true
	return true
end

terra Layer:sync_text_wrap()
	if self.text.wrapped then return end
	self.text.layout:wrap(self.cw)
	self.text.wrapped = true
end

terra Layer:sync_text_align()
	self.text.layout:align(0, 0, self.cw, self.ch, self.text.align_x, self.text.align_y)
	if self.text.selectable then
		self.text.selection:init(&self.text.layout)
	end
end

terra Layer:get_baseline()
	if not self:text_visible() then return self.h end
	return self.text.layout.baseline
end

terra Layer:draw_text(cr: &cairo_t)
	if not self:text_visible() then return end
	var x1: double, y1: double, x2: double, y2: double
	cr:clip_extents(&x1, &y1, &x2, &y2)
	self.text.layout:clip(x1, y1, x2-x1, y2-y1)
	self.text.layout:paint(cr)
end

--[[
function layer:text_bbox()
	if not self:text_visible() then
		return 0, 0, 0, 0
	end
	return self.text.segments:bbox()
end
]]

--text caret & selection drawing ---------------------------------------------

--[[
terra Layer:caret_rect()
	local x, y, w, h = self.text.selection.cursor2:rect(self.caret_width)
	local x, w = self:snapxw(x, w)
	local y, h = self:snapyh(y, h)
	return x, y, w, h
end

terra Layer:caret_visibility_rect()
	local x, y, w, h = self:caret_rect()
	--enlarge the caret rect to contain the line spacing.
	local line = self.text.selection.cursor2.seg.line
	local y = y + line.ascent - line.spaced_ascent
	local h = line.spaced_ascent - line.spaced_descent
	return x, y, w, h
end

terra Layer:draw_caret(cr)
	if not self.focused then return end
	if not self.caret_visible then return end
	local x, y, w, h = self:caret_rect()
	local r, g, b, a = self.ui:rgba(self.caret_color)
	a = a * self.caret_opacity
	cr:rgba(r, g, b, a)
	cr:new_path()
	cr:rectangle(x, y, w, h)
	cr:fill()
end

terra Layer:draw_selection_rect(x, y, w, h, cr)
	cr:rectangle(x, y, w, h)
	cr:fill()
end

terra Layer:draw_text_selection(cr)
	local sel = self.text.selection
	if not sel then return end
	if sel:empty() then return end
	cr:rgba(self.ui:rgba(self.text.selection_color))
	cr:new_path()
	sel:rectangles(self.draw_selection_rect, self, cr)
end

terra Layer:make_visible_caret()
	local segs = self.text.segments
	local lines = segs.lines
	local sx, sy = lines.x, lines.y
	local cw, ch = self:client_size()
	local x, y, w, h = self:caret_visibility_rect()
	lines.x, lines.y = box2d.scroll_to_view(x-sx, y-sy, w, h, cw, ch, sx, sy)
	self:make_visible(self:caret_visibility_rect())
end
]]

terra Layer:draw_text_selection(cr: &cairo_t) end
terra Layer:draw_caret(cr: &cairo_t) end

