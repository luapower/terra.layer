
local low = require'low'
local _M = {__index = low}
setmetatable(_M, _M)
setfenv(1, _M)
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

ALIGN_AUTO          = tr2.ALIGN_AUTO    --only for align_x
ALIGN_LEFT          = tr2.ALIGN_LEFT
ALIGN_RIGHT         = tr2.ALIGN_RIGHT
ALIGN_CENTER        = tr2.ALIGN_CENTER
ALIGN_TOP           = tr2.ALIGN_TOP     --same as ALIGN_LEFT!
ALIGN_BOTTOM        = tr2.ALIGN_BOTTOM  --same as ALIGN_RIGHT!
ALIGN_STRETCH       = tr2.ALIGN_MAX + 1
ALIGN_START         = tr2.ALIGN_MAX + 2 --left for LTR text, right for RTL
ALIGN_END           = tr2.ALIGN_MAX + 3 --right for LTR text, left for RTL
ALIGN_SPACE_EVENLY  = tr2.ALIGN_MAX + 4
ALIGN_SPACE_AROUND  = tr2.ALIGN_MAX + 5
ALIGN_SPACE_BETWEEN = tr2.ALIGN_MAX + 6
ALIGN_BASELINE      = tr2.ALIGN_MAX + 7

local color = cairo_color_t

local struct Layout;

local struct GridCol {
	x: num;
	w: num;
	snap_x: bool;
	fr: num;
	inlayout: bool;
	_min_w: num;
	align_x: enum;
	visible: bool;
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

	background_type: enum; --BACKGROUND_TYPE_*
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

	background_extend: enum; --BACKGROUND_EXTEND_*

	padding_left   : num;
	padding_right  : num;
	padding_top    : num;
	padding_bottom : num;

	visible: bool;
	opacity: num;
	clip_content: enum; --CLIP_CONTENT_*

	text_runs: tr2.TextRuns;
	text_align_x: enum; --ALIGN_*
	text_align_y: enum; --ALIGN_*
	text_segments: tr2.Segs;

	caret_width: num;
	caret_color: color;
	caret_opacity: num;
	caret_insert_mode: bool;

	text_selection: tr2.Selection;
	text_selection_color: color;

	--layouts -----------------------------------------------------------------

	_layout: Layout; --current layout implementation

	_min_w: num;
	_min_h: num;

	min_cw: num;
	min_ch: num;

	align_items_x: enum;  --ALIGN_*
	align_items_y: enum;  --ALIGN_*
 	item_align_x: enum;   --ALIGN_*
	item_align_y: enum;   --ALIGN_*

	align_x: enum; --ALIGN_*
	align_y: enum; --ALIGN_*

	union {

		flex: struct {
			flow: enum; --FLEX_FLOW_*
			wrap: bool;
		}

		grid: struct {
			cols: arr(num);
			rows: arr(num);
			col_gap: num;
			row_gap: num;
			flow: enum; --GRID_FLOW_* mask
			wrap: int;
			min_lines: int;
			_flip_rows: bool;
			_flip_cols: bool;
			_max_row: int;
			_max_col: int;
			_cols: arr(GridCol);
			_rows: arr(GridCol);
		}

	}

	--flex layout element fields
	fr: num;
	break_before: bool;
	break_after : bool;

	--grid layout element fields
	grid_row: int;
	grid_col: int;
	grid_row_span: int;
	grid_col_span: int;

	_grid_row: int;
	_grid_col: int;
	_grid_row_span: int;
	_grid_col_span: int;

}

gettersandsetters(Layer)

--layout plugin interface ----------------------------------------------------

LAYOUT_NULL    = 0
LAYOUT_TEXTBOX = 1
LAYOUT_FLEXBOX = 2
LAYOUT_GRID    = 3

struct Layout {
	type       : enum; --LAYOUT_*
	axis_order : enum; --AXIS_ORDER_*
	sync       : {&Layer} -> {};
	sync_min_w : {&Layer, bool} -> num;
	sync_min_h : {&Layer, bool} -> num;
	sync_x     : {&Layer, bool} -> bool;
	sync_y     : {&Layer, bool} -> bool;
}

--layout interface forwarders
terra Layer:sync_layout()          return self._layout.sync(self) end
terra Layer:sync_min_w(b: bool)    return self._layout.sync_min_w(self, b) end
terra Layer:sync_min_h(b: bool)    return self._layout.sync_min_h(self, b) end
terra Layer:sync_layout_x(b: bool) return self._layout.sync_x(self, b) end
terra Layer:sync_layout_y(b: bool) return self._layout.sync_y(self, b) end

--geometry utils -------------------------------------------------------------

terra Layer:get_pw(): num return self.padding_left + self.padding_right end
terra Layer:get_ph(): num return self.padding_top + self.padding_bottom end
terra Layer:get_cx(): num return self.x + self.padding_left end
terra Layer:get_cy(): num return self.y + self.padding_top end
terra Layer:get_cw(): num return self.w - self.pw end
terra Layer:get_ch(): num return self.h - self.ph end
terra Layer:set_cw(cw: num) self.w = cw + (self.w - self.cw) end
terra Layer:set_ch(ch: num) self.h = ch + (self.h - self.ch) end

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

--text layouting -------------------------------------------------------------

terra Layer:text_visible()
	return self.text_runs.array.len > 0
end

terra Layer:sync_text_shape() return &self.text_segments end
terra Layer:sync_text_wrap() end
terra Layer:sync_text_align() end

terra Layer:get_baseline(): num
	if not self:text_visible() then return self.h end
	return self.text_segments.lines.baseline
end

--layout utils ---------------------------------------------------------------

AXIS_ORDER_XY = 1
AXIS_ORDER_YX = 2

--used by layers that need to solve their layout on one axis completely
--before they can solve it on the other axis. any content-based layout with
--wrapped content is like that: can't know the height until wrapping the
--content which needs to know the width (and viceversa for vertical flow).
terra Layer:sync_layout_separate_axes(axis_order: enum, min_w: num, min_h: num)
	if not self.visible then return end
	axis_order = iif(axis_order ~= 0, axis_order, self._layout.axis_order)
	var sync_x = axis_order == AXIS_ORDER_XY
	var axis_synced = false
	var other_axis_synced = false
	for phase = 0, 3 do
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

--layouting system entry point: called on the top layer.
--called by null-layout layers to layout themselves and their children.
local terra sync(self: &Layer)
	if not self.visible then return end
	self.x, self.w = self:snapxw(self.x, self.w)
	self.y, self.h = self:snapyh(self.y, self.h)
	if self:sync_text_shape() ~= nil then
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
		self:sync_layout()
	end
	return true
end

local null_layout = constant(`Layout {
	type       = LAYOUT_NULL;
	axis_order = 0;
	sync       = sync;
	sync_min_w = sync_min_w;
	sync_min_h = sync_min_h;
	sync_x     = sync_x;
	sync_y     = sync_x;
})

--textbox layout -------------------------------------------------------------

local terra sync(self: &Layer)
	if not self.visible then return end
	var segs = self:sync_text_shape()
	if segs == nil then
		self.cw = 0
		self.ch = 0
		return
	end
	self.cw = max(segs:min_w(), self.min_cw)
	self:sync_text_wrap()
	self.cw = max(segs.lines.max_ax, self.min_cw)
	self.ch = max(self.min_ch, segs.lines.spaced_h)
	self.x, self.w = self:snapxw(self.x, self.w)
	self.y, self.h = self:snapyh(self.y, self.h)
	self:sync_text_align()
	self:sync_layout_children()
end

local terra sync_min_w(self: &Layer, other_axis_synced: bool)
	var min_cw: num
	if not other_axis_synced then --TODO: or self.nowrap
		var segs = self:sync_text_shape()
		min_cw = iif(segs ~= nil, segs:min_w(), 0)
	else
		--height-in-width-out parent layout with wrapping text not supported
		min_cw = 0
	end
	min_cw = max(min_cw, self.min_cw)
	var min_w = snap_up(min_cw + self.pw, self.snap_x)
	self._min_w = min_w
	return min_w
end

local terra sync_min_h(self: &Layer, other_axis_synced: bool)
	var min_ch: num
	if other_axis_synced then --TODO: or self.nowrap
		min_ch = self.text_segments.lines.spaced_h
	else
		--height-in-width-out parent layout with wrapping text not supported
		min_ch = 0
	end
	min_ch = max(min_ch, self.min_ch)
	var min_h = snap_up(min_ch + self.ph, self.snap_y)
	self._min_h = min_h
	return min_h
end

local terra sync_x(self: &Layer, other_axis_synced: bool)
	if not other_axis_synced then
		self:sync_text_wrap()
		return true
	end
end

local terra sync_y(self: &Layer, other_axis_synced: bool)
	if other_axis_synced then
		self:sync_text_align()
		self:sync_layout_children()
		return true
	end
end

local textbox_layout = constant(`Layout {
	type       = LAYOUT_TEXTBOX;
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

local function items_sum_func(T, W)
	local _MIN_W = '_min_'..W
	return terra(items: arr(T), i: int, j: int)
		var sum_w: num = 0
		var item_count = 0
		for i = i, j do
			var item = items:at(i)
			if item.visible then
				sum_w = sum_w + item.[_MIN_W]
				item_count = item_count + 1
			end
		end
		return sum_w, item_count
	end
end

local function items_max_func(T, W)
	local _MIN_W = '_min_'..W
	return terra(items: arr(T), i: int, j: int)
		var max_w: num = 0
		var item_count = 0
		for i = i, j do
			var item = items:at(i)
			if item.visible then
				max_w = max(max_w, item.[_MIN_W])
				item_count = item_count + 1
			end
		end
		return max_w, item_count
	end
end

local function stretch_items_main_axis_func(T, X, W)

	local _MIN_W = '_min_'..W
	local ALIGN_X = 'align_'..X

	--compute a single item's stretched width and aligned width.
	local terra stretched_item_widths(self: &T, total_w: num,
		total_fr: num, total_overflow_w: num, total_free_w: num, align: enum
	)
		var min_w = self.[_MIN_W]
		var flex_w = total_w * self.fr / total_fr
		var sw: num --stretched width
		if min_w > flex_w then --overflow
			sw = min_w
		else
			var free_w = flex_w - min_w
			var free_p = free_w / total_free_w
			var shrink_w = total_overflow_w * free_p
			if shrink_w ~= shrink_w then --total_free_w == 0
				shrink_w = 0
			end
			sw = flex_w - shrink_w
		end
		return sw, iif(align == ALIGN_STRETCH, sw, min_w)
	end

	--stretch a line of items on the main axis.
	local terra stretch_items_main_axis(
		items: arr(T), i: int, j: int, total_w: num, item_align_x: enum,
		moving: bool
		--, set_item_x, set_moving_item_x
	)
		--compute the fraction representing the total width.
		var total_fr = 0
		for i = i, j do
			var item = items:at(i)
			if item.inlayout then
				total_fr = total_fr + max(0, item.fr)
			end
		end
		total_fr = max(1, total_fr) --treat sub-unit fractions like css flexbox

		--compute the total overflow width and total free width.
		var total_overflow_w = 0
		var total_free_w = 0
		for i = i, j do
			var item = items:at(i)
			if item.inlayout then
				var min_w = item.[_MIN_W]
				var flex_w = total_w * max(0, item.fr) / total_fr
				var overflow_w = max(0, min_w - flex_w)
				var free_w = max(0, flex_w - min_w)
				total_overflow_w = total_overflow_w + overflow_w
				total_free_w = total_free_w + free_w
			end
		end

		--compute the stretched width of the moving layer to make room for it.
		--[[
		var moving_layer, moving_x, moving_w, moving_sw
		if moving then
			var layer = items:at(j)
			assert(layer.moving)
			var align = layer.[ALIGN_X] or item_align_x
			var sw, w = stretched_item_widths(
				layer, total_w, total_fr, total_overflow_w, total_free_w, align
			)

			moving_layer = layer
			moving_x = layer[X]
			moving_w = w
			moving_sw = sw
			j = j-1
		end
		]]

		--distribute the overflow to children which have free space to
		--take it. each child shrinks to take in a part of the overflow
		--proportional to its percent of free space.
		var sx: num = 0 --stretched x-coord
		for i = i, j do
			var item = items:at(i)
			if item.inlayout then

				--compute item's stretched width.
				var align = iif(item.[ALIGN_X] ~= 0, item.[ALIGN_X], item_align_x)
				var sw, w = stretched_item_widths(
					item, total_w, total_fr, total_overflow_w, total_free_w, align
				)

				--align item inside the stretched segment defined by (sx, sw).
				var x = sx
				if align == ALIGN_END or align == ALIGN_RIGHT then
					x = sx + sw - w
				elseif align == ALIGN_CENTER then
					x = sx + (sw - w) / 2
				end

				--[[
				if moving_x and moving_x < x + w / 2 then
					set_moving_item_x(moving_layer, i, x, moving_w)

					--reserve space for the moving layer.
					sx = sx + moving_sw
					x = x + moving_sw
					moving_x = false
				end
				]]

				--TODO: set_item_x(layer, x, w, moving)
				sx = sx + sw
			end
		end
	end

	return stretch_items_main_axis
end

--start offset and inter-item spacing for aligning items on the main-axis.
local terra align_metrics(align: enum, container_w: num, items_w: num, item_count: int)
	var x = 0
	var spacing = 0
	if align == ALIGN_END or align == ALIGN_RIGHT then
		x = container_w - items_w
	elseif align == ALIGN_CENTER then
		x = (container_w - items_w) / 2
	elseif align == ALIGN_SPACE_EVENLY then
		spacing = (container_w - items_w) / (item_count + 1)
		x = spacing
	elseif align == ALIGN_SPACE_AROUND then
		spacing = (container_w - items_w) / item_count
		x = spacing / 2
	elseif align == ALIGN_SPACE_BETWEEN then
		spacing = (container_w - items_w) / (item_count - 1)
	end
	return x, spacing
end

--align a line of items on the main axis.
local function align_items_main_axis_func(T, W)
	local _MIN_W = '_min_'..W
	return terra(
		items: arr(T), i: int, j: int, sx: num, spacing: num,
		moving: bool
		--set_item_x, set_moving_item_x
	)
		--compute the spaced width of the moving layer to make room for it.
		--[[
		var moving_layer, moving_x, moving_w, moving_sw
		if moving then
			var layer = items[j]
			assert(layer.moving)
			var w = layer[_MIN_W]

			moving_layer = layer
			moving_x = layer[X]
			moving_w = w
			moving_sw = w + spacing
			j = j-1
		end
		]]

		for i = i, j do
			var item = items:at(i)
			if item.inlayout then
				var x, w = sx, item.[_MIN_W]
				var sw = w + spacing

				--[[
				if moving_x and moving_x < x + w / 2 then
					set_moving_item_x(moving_layer, i, x, moving_w)

					--reserve space for the moving layer.
					sx = sx + moving_sw
					x = x + moving_sw
					moving_x = false
				end
				]]

				--TODO: set_item_x(layer, x, w, moving)
				sx = sx + sw
			end
		end
	end
end

--flexbox layout -------------------------------------------------------------

FLEX_FLOW_X = 1
FLEX_FLOW_Y = 2

--generate pairs of methods for vertical and horizontal flexbox layouts.
local function gen_funcs(X, Y, W, H)

	local CW = 'c'..W
	local CH = 'c'..H
	local _MIN_W = '_min_'..W
	local _MIN_H = '_min_'..H
	local SNAP_X = 'snap_'..X
	local SNAP_Y = 'snap_'..Y

	local ALIGN_ITEMS_X = 'align_items_'..X
	local ALIGN_ITEMS_Y = 'align_items_'..Y
	local ITEM_ALIGN_X = 'item_align_'..X
	local ITEM_ALIGN_Y = 'item_align_'..Y
	local ALIGN_X = 'align_'..X --TODO: unused
	local ALIGN_Y = 'align_'..Y

	local items_sum_x = items_sum_func(Layer, W)
	local items_max_x = items_max_func(Layer, W)
	local stretch_items_main_axis_x = stretch_items_main_axis_func(Layer, X, W)
	local align_items_main_axis_x = align_items_main_axis_func(Layer, W)

	--special items_min_h() for baseline align.
	--requires that the children are already sync'ed on y-axis.
	local terra items_min_h_baseline(self: &Layer, i: int, j: int)
		var max_ascent  = -inf
		var max_descent = -inf
		for i = i, j do
			var layer = self.children:at(i)
			if layer.visible then
				var baseline = layer.baseline
				max_ascent = max(max_ascent, baseline)
				max_descent = max(max_descent, layer._min_h - baseline)
			end
		end
		return max_ascent + max_descent, max_ascent
	end

	local terra items_min_h(self: &Layer, i: int, j: int, align_baseline: bool)
		if align_baseline then
			return items_min_h_baseline(self, i, j)
		end
		return items_max_x(self.children, i, j)
	end

	local terra linewrap_next(self: &Layer, i: int): {int, int}
		i = i + 1
		if i >= self.children.len then
			return -1, -1
		elseif not self.flex.wrap then
			return self.children.len-1, i
		end
		var wrap_w = self.[CW]
		var line_w = 0
		for j = i, self.children.len do
			var layer = self.children:at(j)
			if layer.visible then
				if j > i and layer.break_before then
					return j-1, i
				end
				if layer.break_after then
					return j, i
				end
				var item_w = layer.[_MIN_W]
				if line_w + item_w > wrap_w then
					return j-1, i
				end
				line_w = line_w + item_w
			end
		end
		return self.children.len-1, i
	end

	local struct linewrap {layer: &Layer}
	linewrap.metamethods.__for = function(self, body)
		return quote
			var i: int = 0
			var j: int
			while true do
				j, i = linewrap_next(self.layer, i)
				[ body(i, j) ]
				if j == -1 then break end
			end
		end
	end

	Layer.methods['flexbox_min_cw_'..X] = terra(
		self: &Layer, other_axis_synced: bool, align_baseline: bool
	)
		if self.flex.wrap then
			return items_max_x(self.children, 0, self.children.len)._0
		else
			return items_sum_x(self.children, 0, self.children.len)._0
		end
	end

	Layer.methods['flexbox_min_ch_'..X] = terra(
		self: &Layer, other_axis_synced: bool, align_baseline: bool
	)
		if not other_axis_synced and self.flex.wrap then
			--width-in-height-out parent layout requesting min_w on a y-axis
			--wrapping flexbox (which is a height-in-width-out layout).
			return 0
		end
		var lines_h: num = 0
		for i, j in linewrap{self} do
			var line_h, _ = items_min_h(self, i, j, align_baseline)
			lines_h = lines_h + line_h
		end
		return lines_h
	end

	local terra set_item_x(layer: &Layer, x: num, w: num, moving: bool)
		x, w = snap_xw(x, w, layer.[SNAP_X])
		--TODO
		--var set = moving and layer.transition or layer.end_value
		--set(layer, X, x)
		--set(layer, W, w)
	end

	local terra set_moving_item_x(layer: &Layer, i: int, x: num, w: num)
		--layer[X] = x
		--layer[W] = w
	end

	--stretch and align a line of items on the main axis.
	local terra stretch_items_x(self: &Layer, i: int, j: int, moving: bool)
		stretch_items_main_axis_x(
			self.children, i, j, self.[CW], self.[ITEM_ALIGN_X], moving)
			--TODO: set_item_x, set_moving_item_x)
	end

	--align a line of items on the main axis.
	local terra align_items_x(self: &Layer, i: int, j: int, align: enum, moving: bool)
		if align == ALIGN_STRETCH then
			stretch_items_x(self, i, j, moving)
		else
			var x: num
			var spacing: num
			if align == ALIGN_START or align == ALIGN_LEFT then
				x, spacing = 0, 0
			else
				var items_w, item_count = items_sum_x(self.children, i, j)
				x, spacing = align_metrics(align, self.[CW], items_w, item_count)
			end
			align_items_main_axis_x(
				self.children, i, j, x, spacing, moving)
				--TODO: set_item_x, set_moving_item_x)
		end
	end

	--stretch or align a flexbox's items on the main-axis.
	Layer.methods['flexbox_sync_x_'..X] = terra(
		self: &Layer, other_axis_synced: bool, align_baseline: bool
	)
		var align = self.[ALIGN_ITEMS_X]
		var moving = false --TODO: self.moving_layer and true or false
		for i, j in linewrap{self} do
			align_items_x(self, i, j, align, moving)
		end
		return true
	end

	--align a line of items on the cross-axis.
	local terra align_items_y(self: &Layer, i: int, j: int,
		line_y: num, line_h: num, line_baseline: num
	)
		var snap_y = self.[SNAP_Y]
		var align = self.[ITEM_ALIGN_Y]
		for i = i, j do
			var layer = self.children:at(i)
			if layer.visible then
				var align = layer.[ALIGN_Y] or align
				var y: num
				var h: num
				if align == ALIGN_STRETCH then
					y = line_y
					h = line_h
				else
					var item_h = layer.[_MIN_H]
					if align == ALIGN_TOP or align == ALIGN_START then
						y = line_y
						h = item_h
					elseif align == ALIGN_BOTTOM or align == ALIGN_END then
						y = line_y + line_h - item_h
						h = item_h
					elseif align == ALIGN_CENTER then
						y = line_y + (line_h - item_h) / 2
						h = item_h
					elseif not isnan(line_baseline) then -- <-- TODO
						y = line_baseline - layer.baseline
					end
				end
				if not isnan(line_baseline) then
					y = snap(y, snap_y)
				else
					y, h = snap_xw(y, h, layer.[SNAP_Y])
					--TODO: layer:end_value(H, h)
				end
				--TODO: if not layer.moving then
					--layer:end_value(Y, y)
				--else
					layer.[Y] = y
				--end
			end
		end
	end

	--stretch or align a flexbox's items on the cross-axis.
	Layer.methods['flexbox_sync_y_'..X] = terra(
		self: &Layer, other_axis_synced: bool, align_baseline: bool
	)
		if not other_axis_synced and self.flex.wrap then
			--trying to lay out the y-axis before knowing the x-axis:
			--dismiss and wait for the 3rd pass.
			return false
		end

		var lines_y: num
		var line_spacing: num
		var line_h: num
		var align = self.[ALIGN_ITEMS_Y]
		if align == ALIGN_STRETCH then
			var lines_h = self.[CH]
			var line_count = 0
			for _1,_2 in linewrap{self} do
				line_count = line_count + 1
			end
			line_h = lines_h / line_count
			lines_y = 0
			line_spacing = 0
		elseif align == ALIGN_TOP or align == ALIGN_START then
			lines_y, line_spacing = 0, 0
		else
			var lines_h: num = 0
			var line_count: int = 0
			for i, j in linewrap{self} do
				var line_h, _ = items_min_h(self, i, j, align_baseline)
				lines_h = lines_h + line_h
				line_count = line_count + 1
			end
			lines_y, line_spacing =
				align_metrics(align, self.[CH], lines_h, line_count)
		end
		var y = lines_y
		for i, j in linewrap{self} do
			var line_h = line_h
			var line_baseline: num
			if not isnan(line_h) then
				line_h, line_baseline = items_min_h(self, i, j, align_baseline)
			end
			align_items_y(self, i, j, y, line_h, line_baseline)
			y = y + line_h + line_spacing
		end

		return true
	end

end
gen_funcs('x', 'y', 'w', 'h')
gen_funcs('y', 'x', 'h', 'w')

local terra sync_min_w(self: &Layer, other_axis_synced: bool)

	--sync all children first (bottom-up sync).
	for _,layer in self.children do
		if layer.visible then
			layer:sync_min_w(other_axis_synced) --recurse
		end
	end

	var min_cw = iif(self.flex.flow == FLEX_FLOW_X,
			self:flexbox_min_cw_x(other_axis_synced, false),
			self:flexbox_min_ch_y(other_axis_synced, false))

	min_cw = max(min_cw, self.min_cw)
	var min_w = min_cw + self.pw
	self._min_w = min_w
	return min_w
end

local terra sync_min_h(self: &Layer, other_axis_synced: bool)

	var align_baseline = self.flex.flow == FLEX_FLOW_X
		and self.item_align_y == ALIGN_BASELINE

	--sync all children first (bottom-up sync).
	for _,layer in self.children do
		if layer.visible then
			var item_h = layer:sync_min_h(other_axis_synced) --recurse
			--for baseline align also layout the children because we need
			--their baseline. we can do this here because we already know
			--we won't stretch them beyond their min_h in this case.
			if align_baseline then
				layer.h = snap(item_h, self.snap_y)
				layer:sync_layout_y(other_axis_synced)
			end
		end
	end

	var min_ch = iif(self.flex.flow == FLEX_FLOW_X,
		self:flexbox_min_ch_x(other_axis_synced, align_baseline),
		self:flexbox_min_cw_y(other_axis_synced, align_baseline))

	min_ch = max(min_ch, self.min_ch)
	var min_h = min_ch + self.ph
	self._min_h = min_h
	return min_h
end

local terra sync_x(self: &Layer, other_axis_synced: bool)

	var synced = iif(self.flex.flow == FLEX_FLOW_X,
			self:flexbox_sync_x_x(other_axis_synced, false),
			self:flexbox_sync_y_y(other_axis_synced, false))

	if synced then
		--sync all children last (top-down sync).
		for _,layer in self.children do
			if layer.visible then
				layer:sync_layout_x(other_axis_synced) --recurse
			end
		end
	end
	return synced
end

local terra sync_y(self: &Layer, other_axis_synced: bool)

	if self.flex.flow == FLEX_FLOW_X and self.item_align_y == ALIGN_BASELINE then
		--chilren already sync'ed in sync_min_h().
		return self:flexbox_sync_y_x(other_axis_synced, true)
	end

	var synced = self.flex.flow == FLEX_FLOW_Y
		and self:flexbox_sync_x_y(other_axis_synced, false)
		 or self:flexbox_sync_y_x(other_axis_synced, false)

	if synced then
		--sync all children last (top-down sync).
		for _,layer in self.children do
			if layer.visible then
				layer:sync_layout_y(other_axis_synced) --recurse
			end
		end
	end
	return synced
end

local flexbox_layout = constant(`Layout {
	type       = LAYOUT_FLEXBOX;
	axis_order = AXIS_ORDER_XY;
	sync       = sync;
	sync_min_w = sync_min_w;
	sync_min_h = sync_min_h;
	sync_x     = sync_x;
	sync_y     = sync_x;
})

--[[
--faster hit-testing for non-wrapped flexboxes.
local terra cmp_ys(items, i, y)
	return items[i].visible and items[i].y < y -- < < [=] = < <
end
var terra cmp_xs(items, i, x)
	return items[i].visible and items[i].x < x -- < < [=] = < <
end
terra flexbox:hit_test_flexbox_item(x, y)
	var cmp = self.flex_flow == 'y' and cmp_ys or cmp_xs
	var coord = self.flex_flow == 'y' and y or x
	return max(1, (binsearch(coord, self, cmp) or #self + 1) - 1)
end

terra flexbox:override_hit_test_children(inherited, x, y, reason)
	if #self < 2 or self.flex_wrap then
		return inherited(self, x, y, reason)
	end
	var i = self:hit_test_flexbox_item(x, y)
	return self[i]:hit_test(x, y, reason)
end

--faster clipped drawing for non-wrapped flexboxes.
terra flexbox:override_draw_children(inherited, cr)
	if #self < 1 or self.flex_wrap then
		return inherited(self, cr)
	end
	var x1, y1, x2, y2 = cr:clip_extents()
	var i = self:hit_test_flexbox_item(x1, y1)
	var j = self:hit_test_flexbox_item(x2, y2)
	for i = i, j do
		self[i]:draw(cr)
	end
end
]]

--grid layout ----------------------------------------------------------------

--these flags can be combined: X|Y + L|R + T|B
GRID_FLOW_X = 0; GRID_FLOW_Y = 2 --main axis
GRID_FLOW_L = 0; GRID_FLOW_R = 4 --horizontal direction
GRID_FLOW_T = 0; GRID_FLOW_B = 8 --vertical direction

function GRID_FLOW(s)
	s = s:lower()
	return bor(1,
		(s:find('y', 1, true) and GRID_FLOW_Y or 0),
		(s:find('r', 1, true) and GRID_FLOW_R or 0),
		(s:find('b', 1, true) and GRID_FLOW_B or 0))
end

--digression: bitmap-of-bools object to mark occupied cells.

local struct bitmap {rows: int, cols: int, bits: arr(bool)}

terra bitmap:init()
	self.rows = 0
	self.cols = 0
	self.bits:init()
end

terra bitmap:free()
	self.bits:free()
end

terra bitmap:set(row: int, col: int, val: bool)
	self.bits:set(row * self.cols + col, val)
end

terra bitmap:get(row: int, col: int)
	return self.bits(row * self.cols + col, false)
end

terra bitmap:widen(min_rows: int, min_cols: int)
	var rows = max(1, self.rows)
	var cols = max(1, self.cols)
	while rows < min_rows do rows = rows * 2 end
	while cols < min_cols do cols = cols * 2 end
	if rows > self.rows or cols > self.cols then
		assert(self.bits:resize(rows * cols))
		if cols > self.cols then --move the rows down to widen them
			for row = self.rows-1, -1, -1 do
				copy(
					self.bits:at(row * cols),
					self.bits:at(row * self.cols),
					self.cols)
			end
		end
		self.rows = rows
		self.cols = cols
	end
end

terra bitmap:mark(row1: int, col1: int, row_span: int, col_span: int)
	var row2 = row1 + row_span - 1
	var col2 = col1 + col_span - 1
	self:widen(row2, col2)
	for row = row1, row2 do
		for col = col1, col2 do
			self:set(row, col, true)
		end
	end
end

terra bitmap:check(row1: int, col1: int, row_span: int, col_span: int)
	var row2 = row1 + row_span - 1
	var col2 = col1 + col_span - 1
	for row = row1, row2 do
		for col = col1, col2 do
			if self:get(row, col) then
				return true
			end
		end
	end
	return false
end

--auto-positioning algorithm

local terra clip_span(
	row1: int, col1: int, row_span: int, col_span: int,
	max_row: int, max_col: int
)
	var row2 = row1 + row_span - 1
	var col2 = col1 + col_span - 1
	--clip the span to grid boundaries
	row1 = clamp(row1, 1, max_row)
	col1 = clamp(col1, 1, max_col)
	row2 = clamp(row2, 1, max_row)
	col2 = clamp(col2, 1, max_col)
	--support negative spans
	if row1 > row2 then
		row1, row2 = row2, row1
	end
	if col1 > col2 then
		col1, col2 = col2, col1
	end
	row_span = row2 - row1 + 1
	col_span = col2 - col1 + 1
	return row1, col1, row_span, col_span
end

terra Layer:sync_layout_grid_autopos()

	var flow = self.grid.flow
	var col_first = (flow and GRID_FLOW_Y) == 0
	var row_first = not col_first
	var flip_cols = (flow and GRID_FLOW_R) ~= 0
	var flip_rows = (flow and GRID_FLOW_B) ~= 0
	var grid_wrap = max(1, self.grid.wrap)
	var max_col = iif(col_first, grid_wrap, self.grid.min_lines)
	var max_row = iif(row_first, grid_wrap, self.grid.min_lines)

	var occupied: bitmap; occupied:init()

	--position explicitly-positioned layers first and mark occupied cells.
	--grow the grid bounds to include layers outside (grid.wrap, grid.min_lines).
	var missing_indices = false
	var negative_indices = false
	for _,layer in self.children do
		if layer.visible then
			var row = layer.grid_row
			var col = layer.grid_col
			var row_span = max(1, layer.grid_row_span)
			var col_span = max(1, layer.grid_col_span)

			if row ~= 0 or col ~= 0 then --explicit position
				row = max(1, row)
				col = max(1, col)
				if row > 0 and col > 0 then
					row, col, row_span, col_span =
						clip_span(row, col, row_span, col_span, 1/0, 1/0)

					occupied:mark(row, col, row_span, col_span)

					max_row = max(max_row, row + row_span - 1)
					max_col = max(max_col, col + col_span - 1)
				else
					negative_indices = true --solve these later
				end
			else --auto-positioned
				--negative spans are treated as positive.
				row_span = abs(row_span)
				col_span = abs(col_span)

				--grow the grid bounds on the main axis to fit the widest layer.
				if col_first then
					max_col = max(max_col, col_span)
				else
					max_row = max(max_row, row_span)
				end

				missing_indices = true --solve these later
			end

			layer._grid_row = row
			layer._grid_col = col
			layer._grid_row_span = row_span
			layer._grid_col_span = col_span
		end
	end

	--position explicitly-positioned layers with negative indices
	--now that we know the grid bounds. these types of spans do not enlarge
	--the grid bounds, but instead are clipped to it.
	if negative_indices then
		for _,layer in self.children do
			if layer.visible then
				var row = layer._grid_row
				var col = layer._grid_col
				if row < 0 or col < 0 then
					var row_span = layer._grid_row_span
					var col_span = layer._grid_col_span
					if row < 0 then
						row = max_row + row + 1
					end
					if col < 0 then
						col = max_col + col + 1
					end
					row, col, row_span, col_span =
						clip_span(row, col, row_span, col_span, max_row, max_col)

					occupied:mark(row, col, row_span, col_span)

					layer._grid_row = row
					layer._grid_col = col
					layer._grid_row_span = row_span
					layer._grid_col_span = col_span
				end
			end
		end
	end

	--auto-wrap layers with missing explicit indices over non-occupied cells.
	--grow grid bounds on the cross-axis if needed but not on the main axis.
	--these types of spans are never clipped to the grid bounds.
	if missing_indices then
		var row, col = 1, 1
		for _,layer in self.children do
			if layer.visible and layer._grid_row == 0 then
				var row_span = layer._grid_row_span
				var col_span = layer._grid_col_span

				while true do
					--check for wrapping.
					if col_first and col + col_span - 1 > max_col then
						col = 1
						row = row + 1
					elseif row_first and row + row_span - 1 > max_row then
						row = 1
						col = col + 1
					end
					if occupied:check(row, col, row_span, col_span) then
						--advance cursor by one cell.
						if col_first then
							col = col + 1
						else
							row = row + 1
						end
					else
						break
					end
				end

				occupied:mark(row, col, row_span, col_span)

				layer._grid_row = row
				layer._grid_col = col

				--grow grid bounds on the cross-axis.
				if col_first then
					max_row = max(max_row, row + row_span - 1)
				else
					max_col = max(max_col, col + col_span - 1)
				end

				--advance cursor to right after the span, without wrapping.
				if col_first then
					col = col + col_span
				else
					row = row + row_span
				end
			end
		end
	end

	--reverse the order of rows and/or columns depending on grid_flow.
	if flip_rows or flip_cols then
		for _,layer in self.children do
			if layer.visible then
				if flip_rows then
					layer._grid_row = max_row
						- layer._grid_row
						- layer._grid_row_span
						+ 2
				end
				if flip_cols then
					layer._grid_col = max_col
						- layer._grid_col
						- layer._grid_col_span
						+ 2
				end
			end
		end
	end

	occupied:free()

	self.grid._flip_rows = flip_rows
	self.grid._flip_cols = flip_cols
	self.grid._max_row = max_row
	self.grid._max_col = max_col
end

--layouting algorithm

local stretch_items_main_axis = stretch_items_main_axis_func(GridCol, 'x', 'w')
local items_sum = items_sum_func(GridCol, 'w')
local align_items_main_axis = align_items_main_axis_func(GridCol, 'w')

local function gen_funcs(X, Y, W, H, COL)

	local CW = 'c'..W
	local PW = 'p'..W
	local MIN_CW = 'min_'..CW
	local _MIN_W = '_min_'..W
	local SNAP_X = 'snap_'..X
	local COLS = COL..'s'
	local COL_GAP = COL..'_gap'
	local ALIGN_ITEMS_X = 'align_items_'..X
	local ITEM_ALIGN_X = 'item_align_'..X
	local ALIGN_X = 'align_'..X
	local _COLS = '_'..COLS
	local _MAX_COL = '_max_'..COL
	local _COL = '_grid_'..COL
	local _COL_SPAN = '_grid_'..COL..'_span'
	local _FLIP_COLS = '_flip_'..COL..'s'

	local terra sync_min_w(self: &Layer, other_axis_synced: bool)

		--sync all children first (bottom-up sync).
		for _,layer in self.children do
			if layer.visible then
				layer:['sync_min_'..W](other_axis_synced) --recurse
			end
		end

		var gap_w = self.grid.[COL_GAP]
		var max_col = self.grid.[_MAX_COL]
		var fr = self.grid.[COLS] --{fr1, ...}

		--compute the fraction representing the total width.
		var total_fr = 0.0
		for _,layer in self.children do
			if layer.inlayout then
				var col1 = layer.[_COL]
				var col2 = col1 + layer.[_COL_SPAN]
				for col = col1, col2 do
					total_fr = total_fr + fr:get(col, 1)
				end
			end
		end

		--create pseudo-layers to apply flexbox stretching to.
		var cols = self.grid.[_COLS]
		assert(cols:resize(max_col))

		for col = 0, max_col do
			cols:set(col, GridCol{
				inlayout = true,
				fr = fr(col),
				_min_w = 0,
				x = 0,
				w = 0,
				snap_x = self.[SNAP_X],
			})
		end

		--compute the minimum widths for each column.
		for _,layer in self.children do
			if layer.inlayout then
				var col1 = layer.[_COL]
				var col2 = col1 + layer.[_COL_SPAN] - 1
				var span_min_w = layer.[_MIN_W]

				var gap_col1: num
				if col2 == 1 and col2 == max_col then
					gap_col1 = 0
				elseif (col2 == 1 or col2 == max_col) then
					gap_col1 = gap_w * .5
				else
					gap_col1 = gap_w
				end

				var gap_col2: num
				if col2 == 1 and col2 == max_col then
					gap_col2 = 0
				elseif (col2 == 1 or col2 == max_col) then
					gap_col2 = gap_w * .5
				else
					gap_col2 = gap_w
				end

				if col1 == col2 then
					var item = cols:at(col1)
					var col_min_w = span_min_w + gap_col1 + gap_col2
					item._min_w = max(item._min_w, col_min_w)
				else --merged columns: unmerge
					var span_fr = 0
					for col = col1, col2 do
						span_fr = span_fr + fr(col, 1)
					end
					for col = col1, col2 do
						var item = cols:at(col)
						var col_min_w = fr(col, 1) / span_fr * span_min_w
						col_min_w = col_min_w
							+ iif(col == col1, gap_col1, 0.0)
							+ iif(col == col2, gap_col2, 0.0)
						item._min_w = max(item._min_w, col_min_w)
					end
				end
			end
		end

		var min_cw = 0
		for _,item in cols do
			min_cw = min_cw + item._min_w
		end

		min_cw = max(min_cw, self.[MIN_CW])
		var min_w = min_cw + self.[PW]
		self.[_MIN_W] = min_w
		return min_w
	end

	--[[
	local terra set_item_x(layer, x, w, moving)
		x, w = snap_xw(x, w, layer[SNAP_X])
		var set = moving and layer.transition or layer.end_value
		set(layer, X, x)
		set(layer, W, w)
	end

	local terra set_moving_item_x(layer, i, x, w)
		--moving NYI
	end
	]]

	local terra sync_x(self: &Layer, other_axis_synced: bool)

		var cols = self.grid.[_COLS]
		var gap_w = self.grid.[COL_GAP]
		var container_w = self.[CW]
		var align_items_x = self.[ALIGN_ITEMS_X]
		var item_align_x = self.[ITEM_ALIGN_X]
		var snap_x = self.[SNAP_X]

		var ALIGN_START, ALIGN_END = ALIGN_START, ALIGN_END
		if self.grid.[_FLIP_COLS] then
			ALIGN_START, ALIGN_END = ALIGN_END, ALIGN_START
		end

		if align_items_x == ALIGN_STRETCH then
			stretch_items_main_axis(
				cols, 0, cols.len, container_w, ALIGN_STRETCH, false)
				--set_item_x, set_moving_item_x,
				--X, W, ALIGN_END, ALIGN_RIGHT)
		else
			var items_w, item_count = items_sum(cols, 0, cols.len)
			var x: num
			var spacing: num
			if align_items_x == ALIGN_START or align_items_x == ALIGN_LEFT then
				x, spacing = 0, 0
			else
				x, spacing = align_metrics(align_items_x, self.[CW], items_w, item_count)
			end
			align_items_main_axis(cols, 0, cols.len, x, spacing, false)
				--set_item_x, set_moving_item_x,
		end

		var x = 0
		for _,layer in self.children do
			if layer.inlayout then

				var col1 = layer.[_COL]
				var col2 = col1 + layer.[_COL_SPAN] - 1
				var col_item1 = cols:at(col1)
				var col_item2 = cols:at(col2)
				var x1 = col_item1.x
				var x2 = col_item2.x + col_item2.w

				var gap1 = iif(col1 == 1, 0.0, gap_w * .5)
				var gap2 = iif(col2 == cols.len, 0.0, gap_w * .5)
				x1 = x1 + gap1
				x2 = x2 - gap2

				var align = iif(layer.[ALIGN_X] ~= 0, layer.[ALIGN_X], item_align_x)
				var x: num, w: num
				if align == ALIGN_STRETCH then
					x, w = x1, x2 - x1
				elseif align == ALIGN_START or align == ALIGN_LEFT then
					x, w = x1, layer.[_MIN_W]
				elseif align == ALIGN_END or align == ALIGN_RIGHT then
					w = layer.[_MIN_W]
					x = x2 - w
				elseif align == ALIGN_CENTER then
					w = layer.[_MIN_W]
					x = x1 + (x2 - x1 - w) / 2
				end
				layer.[X], layer.[W] = snap_xw(x, w, snap_x)
			end
		end

		--sync all children last (top-down sync).
		for _,layer in self.children do
			if layer.visible then
				layer:['sync_layout_'..X](other_axis_synced) --recurse
			end
		end
		return true
	end

	return sync_min_w, sync_x
end
local sync_min_w, sync_x = gen_funcs('x', 'y', 'w', 'h', 'col', ALIGN_LEFT, ALIGN_RIGHT)
local sync_min_h, sync_y = gen_funcs('y', 'x', 'h', 'w', 'row', ALIGN_TOP, ALIGN_BOTTOM)

local terra sync(self: &Layer)
	self:sync_layout_grid_autopos()
	self:sync_layout_separate_axes(0, -inf, -inf)
end

local grid_layout = constant(`Layout {
	type       = LAYOUT_GRID;
	axis_order = AXIS_ORDER_XY;
	sync       = sync;
	sync_min_w = sync_min_w;
	sync_min_h = sync_min_h;
	sync_x     = sync_x;
	sync_y     = sync_x;
})

--layout plugin vtable -------------------------------------------------------

--NOTE: layouts must be added in the order of LAYOUT_* constants.
local layouts = constant(`arrayof(Layout,
	null_layout,
	textbox_layout,
	flexbox_layout,
	grid_layout
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
	self.align_items_x = ALIGN_STRETCH
	self.align_items_y = ALIGN_STRETCH
	self.item_align_x  = ALIGN_STRETCH
	self.item_align_y  = ALIGN_STRETCH

end

terra Layer:free(): {}
	self.children:free()
	self.border_dash:free()
end

print(sizeof(Layer))
