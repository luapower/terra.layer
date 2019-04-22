
--children geometry, drawing & hit testing

setfenv(1, require'layerlib_types')
require'layerlib_border'
require'layerlib_background'
require'layerlib_text'
require'layerlib_shadow'

--[[
function layer:children_bbox(strict)
	local x, y, w, h = 0, 0, 0, 0
	for _,layer in ipairs(self) do
		x, y, w, h = box2d.bounding_box(x, y, w, h,
			layer:bbox(strict))
	end
	return x, y, w, h
end
]]

terra Layer.methods.draw :: {&Layer, &cairo_t} -> {}

terra Layer:draw_children(cr: &cairo_t): {} --called in content space
	for _,layer in self.children do
		layer:draw(cr)
	end
end

--[[
function layer:hit_test_children(x, y, reason) --called in content space
	for i = #self, 1, -1 do
		local widget, area = self[i]:hit_test(x, y, reason)
		if widget then
			return widget, area
		end
	end
end
]]

--content drawing & hit testing

terra Layer:draw_content(cr: &cairo_t) --called in own content space
	self:draw_children(cr)
	self:draw_text_selection(cr)
	self:draw_text(cr)
	self:draw_caret(cr)
end

--[[
function Layer:hit_test_content(x, y, reason) --called in own content space
	local widget, area = self:hit_test_text(x, y, reason)
	if not widget then
		return self:hit_test_children(x, y, reason)
	end
	return widget, area
end

function Layer:content_bbox(strict)
	local x, y, w, h = self:children_bbox(strict)
	return box2d.bounding_box(x, y, w, h, self:text_bbox())
end
]]

CLIP_CONTENT_NOCLIP        = 0
CLIP_CONTENT_TO_PADDING    = 1
CLIP_CONTENT_TO_BACKGROUND = 1

terra Layer:draw(cr: &cairo_t) --called in parent's content space; child intf.

	if not self.visible or self.opacity <= 0 then
		return
	end

	var compose = self.opacity < 1
	if compose then
		cr:push_group()
	else
		cr:save()
	end

	var m = self:cr_abs_matrix(cr)
	cr:matrix(&m)

	var cc = self.clip_content ~= CLIP_CONTENT_NOCLIP
	var b = self.background
	var bg = b.type ~= BACKGROUND_TYPE_NONE

	self:draw_shadow(cr)

	var clip = bg or cc
	if clip then
		cr:save()
		cr:new_path()
		self:background_path(cr, 0) --'background' clipping is implicit here
		cr:clip()
		if bg then
			self:paint_background(b, cr)
		end
		if cc == true then
			cr:new_path()
			cr:rectangle(self.cx, self.cy, self.cw, self.ch)
			cr:clip()
		elseif not cc then --clip was only needed to draw the bg
			cr:restore()
			clip = false
		end
	end
	if not cc then
		self:draw_border(cr)
	end
	cr:translate(self.cx, self.cy)
	self:draw_content(cr)
	cr:translate(-self.cx, -self.cy)
	if clip then
		cr:restore()
	end

	if cc then
		self:draw_border(cr)
	end

	if compose then
		cr:pop_group_to_source()
		cr:paint_with_alpha(self.opacity)
		cr:rgb(0, 0, 0) --release source
	else
		cr:restore()
	end
end

