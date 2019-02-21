
local low = require'low'
local layer = {__index = low}
setmetatable(layer, layer)
setfenv(1, layer)
require'cairo2'
require'tr2_paint_cairo'
local tr2 = require'tr2'

--utils ----------------------------------------------------------------------

local terra snap(x: num, enable: bool)
	return iif(enable, floor(x + .5), x)
end

local terra snap_xw(x: num, w: num, enable: bool)
	if not enable then return x, w end
	var x1 = floor(x + .5)
	var x2 = floor(x + w + .5)
	return x1, x2 - x1
end

local terra snap_up(x: num, enable: bool)
	return iif(enable, ceil(x), x)
end

--types ----------------------------------------------------------------------

AXIS_ORDER_XY = 0
AXIS_ORDER_YX = 1

local color = cairo_color_t
local nocolor = `color{0, 0, 0, 0}

LAYOUT_NULL    = 0
LAYOUT_TEXTBOX = 1
LAYOUT_FLEXBOX = 2
LAYOUT_GRID    = 3

local struct Layout {
	type       : enum;
	axis_order : enum;
	sync       : {&Layer} -> {};
	sync_min_w : {&Layer, bool} -> num;
	sync_min_h : {&Layer, bool} -> num;
	sync_x     : {&Layer, bool} -> bool;
	sync_y     : {&Layer, bool} -> bool;
}

local struct Layer {

	parent: &Layer;
	children: arr(Layer);

	x: num;
	y: num;
	w: num;
	h: num;

	rotation: num;
	rotation_cx: num;
	rotation_cy: num;

	scale: num;
	scale_cx: num;
	scale_cy: num;

	snap_x: bool;
	snap_y: bool;

	border_width_left   : num;
	border_width_right  : num;
	border_width_top    : num;
	border_width_bottom : num;

	corner_radius_top_left     : num;
	corner_radius_top_right    : num;
	corner_radius_bottom_left  : num;
	corner_radius_bottom_right : num;

	border_color_left   : color;
	border_color_right  : color;
	border_color_top    : color;
	border_color_bottom : color;

	border_dash: arr(double);
	border_dash_offset: int;

	border_offset: num;

	background_type: enum;
	union {

		background_color: color;

		background_linear_gradient: struct {
			x1: num;
			y1: num;
			x2: num;
			y2: num;
			colors: arr(num);
		}

		background_radial_gradient: struct {
			cx1: num;
			cy1: num;
			r1 : num;
			cx2: num;
			cy2: num;
			r2 : num;
			colors: arr(num);
		}

		--background_image: bitmap;
	}

	background_hittable: bool;

	background_x: num;
	background_y: num;

	background_rotation: num;
	background_rotation_cx: num;
	background_rotation_cy: num;

	background_scale: num;
	background_scale_cx: num;
	background_scale_cy: num;

	background_extend: enum;

	padding_left   : num;
	padding_right  : num;
	padding_top    : num;
	padding_bottom : num;

	visible: bool;
	opacity: num;
	clip_content: enum;

	text_runs: tr2.TextRuns;
	text_align_x: enum;
	text_align_y: enum;

	caret_width: num;
	caret_color: color;
	caret_opacity: num;
	caret_insert_mode: bool;

	text_selection: tr2.Selection;
	text_selection_color: color;

	_layout: Layout;

	_min_w: num;
	_min_h: num;

	min_cw: num;
	min_ch: num;

	align_items_x: enum;
	align_items_y: enum;
	item_align_x: enum;
	item_align_y: enum;

	align_x: enum;
	align_y: enum;

	union {

		flex: struct {
			flow: enum;
			wrap: bool;
		}

		grid: struct {
			cols: arr(num);
			rows: arr(num);
			col_gap: num;
			row_gap: num;
			flow: enum;
			wrap: int;
			min_lines: int;
		}

	}

	fr: num;

	grid_row: int;
	grid_col: int;
	grid_row_span: int;
	grid_col_span: int;


}

gettersandsetters(Layer)

--geometry utils -------------------------------------------------------------

terra Layer:get_pw(): num return self.padding_left + self.padding_right end
terra Layer:get_ph(): num return self.padding_top + self.padding_bottom end
terra Layer:get_cx(): num return self.x + self.padding_left end
terra Layer:get_cy(): num return self.y + self.padding_top end
terra Layer:get_cw(): num return self.w - self.pw end
terra Layer:get_ch(): num return self.h - self.ph end

terra Layer:snapx(x: num) return snap(x, self.snap_x) end
terra Layer:snapy(y: num) return snap(y, self.snap_y) end
terra Layer:snapxw(x: num, w: num) return snap_xw(x, w, self.snap_x) end
terra Layer:snapyh(y: num, h: num) return snap_xw(y, h, self.snap_y) end
terra Layer:snapcx(cx: num) return snap(cx-self.cx, self.snap_x)+self.cx end
terra Layer:snapcy(cy: num) return snap(cy-self.cy, self.snap_y)+self.cy end

--drawing & hit-testing ------------------------------------------------------

terra Layer:draw(cr: &cairo_t)

end

terra Layer:hit_test(cr: &cairo_t, x: num, y: num): {&Layer, enum}

end

--text -----------------------------------------------------------------------

terra Layer:sync_text_shape() return true end
terra Layer:sync_text_wrap() end
terra Layer:sync_text_align() end

--layouting ------------------------------------------------------------------

terra Layer:sync_layout()          return self._layout.sync(self) end
terra Layer:sync_min_w(b: bool)    return self._layout.sync_min_w(self, b) end
terra Layer:sync_min_h(b: bool)    return self._layout.sync_min_h(self, b) end
terra Layer:sync_layout_x(b: bool) return self._layout.sync_x(self, b) end
terra Layer:sync_layout_y(b: bool) return self._layout.sync_y(self, b) end

--used by layers that need to solve their layout on one axis completely
--before they can solve it on the other axis. any content-based layout with
--wrapped content is like that: can't know the height until wrapping the
--content which needs to know the width (and viceversa for vertical flow).
terra Layer:sync_layout_separate_axes(axis_order: enum, min_w: num, min_h: num)
	if not self.visible then return end
	var sync_x = iif(axis_order ~= -1, axis_order, self._layout.axis_order) == AXIS_ORDER_XY
	var axis_synced = false
	var other_axis_synced = false
	for phase = 1, 3 do
		other_axis_synced = axis_synced
		if sync_x then
			--sync the x-axis.
			self.w = max(self:sync_min_w(other_axis_synced), min_w)
			axis_synced = self:sync_layout_x(other_axis_synced)
		else
			--sync the y-axis.
			self.h = max(self:sync_min_h(other_axis_synced), min_h)
			axis_synced = self:sync_layout_y(other_axis_synced)
		end
		if axis_synced and other_axis_synced then
			break --both axes were solved before last phase.
		end
		sync_x = not sync_x
	end
	assert(axis_synced and other_axis_synced)
end

terra Layer:sync_layout_children()
	for _,layer in self.children do
		layer:sync_layout() --recurse
	end
end

--null layout ----------------------------------------------------------------

--layouting system entry point: called on the window view layer.
--called by null-layout layers to layout themselves and their children.
local terra sync(self: &Layer)
	if not self.visible then return end
	self.x, self.w = self:snapxw(self.x, self.w)
	self.y, self.h = self:snapyh(self.y, self.h)
	if self:sync_text_shape() then
		self:sync_text_wrap()
		self:sync_text_align()
	end
	self:sync_layout_children()
end

--called by flexible layouts to know the minimum width of their children.
--width-in-height-out layouts call this before h and y are sync'ed.
local terra sync_min_w(self: &Layer, other_axis_synced: bool)
	self._min_w = snap_up(self.min_cw + self.pw, self.snap_x)
	return self._min_w
end

--called by flexible layouts to know the minimum height of their children.
--width-in-height-out layouts call this only after w and x are sync'ed.
local terra sync_min_h(self: &Layer, other_axis_synced: bool)
	self._min_h = snap_up(self.min_ch + self.ph, self.snap_y)
	return self._min_h
end

--called by flexible layouts to sync their children on one axis. in response,
--null-layouts sync themselves and their children on both axes when the
--second axis is synced.
local terra sync_x(self: &Layer, other_axis_synced: bool)
	if other_axis_synced then
		sync(self)
	end
	return true
end

local null_layout = constant(`Layout {
	axis_order = 0;
	sync       = sync;
	sync_min_w = sync_min_w;
	sync_min_h = sync_min_h;
	sync_x     = sync_x;
	sync_y     = sync_x;
})

--stuff common to flexbox & grid layouts -------------------------------------

terra Layer:get_inlayout()
	return self.visible --TODO: and (not self.dragging or self.moving)
end

--[[
local terra items_sum(self: &Layer, i: int, j: int, _MIN_W)
	local sum_w = 0
	local item_count = 0
	for i = i, j do
		local layer = self[i]
		if layer.visible then
			sum_w = sum_w + layer[_MIN_W]
			item_count = item_count + 1
		end
	end
	return sum_w, item_count
end

local function items_max(self, i, j, _MIN_W)
	local max_w = 0
	local item_count = 0
	for i = i, j do
		local layer = self[i]
		if layer.visible then
			max_w = max(max_w, layer[_MIN_W])
			item_count = item_count + 1
		end
	end
	return max_w, item_count
end

--compute a single item's stretched width and aligned width.
local function stretched_item_widths(
	layer, total_w, total_fr, total_overflow_w, total_free_w, align,
	_MIN_W
)
	local min_w = layer[_MIN_W]
	local flex_w = total_w * layer.fr / total_fr
	local sw --stretched width
	if min_w > flex_w then --overflow
		sw = min_w
	else
		local free_w = flex_w - min_w
		local free_p = free_w / total_free_w
		local shrink_w = total_overflow_w * free_p
		if shrink_w ~= shrink_w then --total_free_w == 0
			shrink_w = 0
		end
		sw = flex_w - shrink_w
	end
	return sw, align == 'stretch' and sw or min_w
end

--stretch a line of items on the main axis.
local function stretch_items_main_axis(
	items, i, j, total_w, item_align_x, moving,
	set_item_x, set_moving_item_x,
	X, W, _MIN_W, ALIGN_X, END, RIGHT
)
	--compute the fraction representing the total width.
	local total_fr = 0
	for i = i, j do
		local layer = items[i]
		if layer.inlayout then
			total_fr = total_fr + max(0, layer.fr)
		end
	end
	total_fr = max(1, total_fr) --treat sub-unit fractions like css flexbox

	--compute the total overflow width and total free width.
	local total_overflow_w = 0
	local total_free_w = 0
	for i = i, j do
		local layer = items[i]
		if layer.inlayout then
			local min_w = layer[_MIN_W]
			local flex_w = total_w * max(0, layer.fr) / total_fr
			local overflow_w = max(0, min_w - flex_w)
			local free_w = max(0, flex_w - min_w)
			total_overflow_w = total_overflow_w + overflow_w
			total_free_w = total_free_w + free_w
		end
	end

	--compute the stretched width of the moving layer to make room for it.
	local moving_layer, moving_x, moving_w, moving_sw
	if moving then
		local layer = items[j]
		assert(layer.moving)
		local align = layer[ALIGN_X] or item_align_x
		local sw, w = stretched_item_widths(
			layer, total_w, total_fr, total_overflow_w, total_free_w, align,
			_MIN_W
		)

		moving_layer = layer
		moving_x = layer[X]
		moving_w = w
		moving_sw = sw
		j = j-1
	end

	--distribute the overflow to children which have free space to
	--take it. each child shrinks to take in a part of the overflow
	--proportional to its percent of free space.
	local sx = 0 --stretched x-coord
	for i = i, j do
		local layer = items[i]
		if layer.inlayout then

			--compute item's stretched width.
			local align = layer[ALIGN_X] or item_align_x
			local sw, w = stretched_item_widths(
				layer, total_w, total_fr, total_overflow_w, total_free_w, align,
				_MIN_W
			)

			--align item inside the stretched segment defined by (sx, sw).
			local x = sx
			if align == END or align == RIGHT then
				x = sx + sw - w
			elseif align == 'center' then
				x = sx + (sw - w) / 2
			end

			if moving_x and moving_x < x + w / 2 then
				set_moving_item_x(moving_layer, i, x, moving_w)

				--reserve space for the moving layer.
				sx = sx + moving_sw
				x = x + moving_sw
				moving_x = false
			end

			set_item_x(layer, x, w, moving)
			sx = sx + sw
		end
	end
end

--start offset and inter-item spacing for aligning items on the main-axis.
local function align_metrics(
	align, container_w, items_w, item_count,
	END, RIGHT
)
	local x = 0
	local spacing = 0
	if align == END or align == RIGHT then
		x = container_w - items_w
	elseif align == 'center' then
		x = (container_w - items_w) / 2
	elseif align == 'space_evenly' then
		spacing = (container_w - items_w) / (item_count + 1)
		x = spacing
	elseif align == 'space_around' then
		spacing = (container_w - items_w) / item_count
		x = spacing / 2
	elseif align == 'space_between' then
		spacing = (container_w - items_w) / (item_count - 1)
	end
	return x, spacing
end

--align a line of items on the main axis.
local function align_items_main_axis(
	items, i, j, sx, spacing, moving,
	set_item_x, set_moving_item_x,
	X, W, _MIN_W
)
	--compute the spaced width of the moving layer to make room for it.
	local moving_layer, moving_x, moving_w, moving_sw
	if moving then
		local layer = items[j]
		assert(layer.moving)
		local w = layer[_MIN_W]

		moving_layer = layer
		moving_x = layer[X]
		moving_w = w
		moving_sw = w + spacing
		j = j-1
	end

	for i = i, j do
		local layer = items[i]
		if layer.inlayout then
			local x, w = sx, layer[_MIN_W]
			local sw = w + spacing

			if moving_x and moving_x < x + w / 2 then
				set_moving_item_x(moving_layer, i, x, moving_w)

				--reserve space for the moving layer.
				sx = sx + moving_sw
				x = x + moving_sw
				moving_x = false
			end

			set_item_x(layer, x, w, moving)

			sx = sx + sw
		end
	end
end
]]

local layouts = constant(`arrayof(Layout,
	null_layout
	--textbox_layout,
	--flexbox_layout,
	--grid_layout,
))

terra Layer:get_layout() return self._layout.type end

terra Layer:set_layout(type: enum)
	self._layout = layouts[type]
end

--init -----------------------------------------------------------------------

terra Layer:init()
	fill(self)
	self.children:init()
	self.border_dash:init()
	self.layout = LAYOUT_NULL
end


print(sizeof(Layer))
