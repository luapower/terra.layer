
--if not ... then require'layer2_demo'; return end

local low = require'low'
local _M = {__index = low}
setmetatable(_M, _M)
setfenv(1, _M)
require'cairo2'
require'tr2_paint_cairo'
local tr2 = require'tr2'

--types ----------------------------------------------------------------------

color = cairo_color_t --(double)r,g,b,a
color.metamethods.__eq = macro(function(c1, c2)
	return `
		    c1._0 == c2._0
		and c1._1 == c2._1
		and c1._2 == c2._2
		and c1._3 == c2._3
end)
local props = addproperties(color)
props.alpha = macro(function(self) return `self._3 end)

matrix = cairo_matrix_t

struct BoolBitmap {
	rows: int;
	cols: int;
	bits: arr(bool);
}

struct Layer;

struct Layout {
	type       : enum; --LAYOUT_*
	axis_order : enum; --AXIS_ORDER_*
	init       : {&Layer} -> {};
	free       : {&Layer} -> {};
	sync       : {&Layer} -> {};
	sync_min_w : {&Layer, bool} -> num;
	sync_min_h : {&Layer, bool} -> num;
	sync_x     : {&Layer, bool} -> bool;
	sync_y     : {&Layer, bool} -> bool;
}

struct CaretProperties {
	width: num;
	color: color;
}

struct LayerManager {
	tr: tr2.TextRenderer;
	grid_occupied: BoolBitmap;
	layers: freelist(Layer);
	caret: CaretProperties;
}

struct FlexboxLayoutProperties {
	flow: enum; --FLEX_FLOW_*
	wrap: bool;
}

struct GridLayoutCol {
	x: num;
	w: num;
	fr: num;
	align_x: enum;
	_min_w: num;
	snap_x: bool;
	inlayout: bool;
	--visible: bool;
}

struct GridLayoutProperties {
	cols: arr(num);
	rows: arr(num);
	col_gap: num;
	row_gap: num;
	flow: enum; --GRID_FLOW_* mask
	wrap: int;
	min_lines: int;
	--computed by the auto-positioning algorithm.
	_flip_rows: bool;
	_flip_cols: bool;
	_max_row: int;
	_max_col: int;
	_cols: arr(GridLayoutCol);
	_rows: arr(GridLayoutCol);
}

struct ColorStop {
	offset: num;
	color: color;
}

struct LinearGradient {
	x1: num; y1: num;
	x2: num; y2: num;
	color_stops: arr(ColorStop);
}

struct RadialGradient {
	cx1: num; cy1: num; r1: num;
	cx2: num; cy2: num; r2: num;
	color_stops: arr(ColorStop);
}

struct Bitmap {
	w: int;
	h: int;
	stride: int;
	format: enum; --BITMAP_FORMAT_*
	pixels: &opaque;
}

struct Layer {

	_manager: &LayerManager;

	_parent: &Layer;
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

	--draw rounded corners with a modified bezier for smoother line-to-arc
	--transitions. kappa=1 uses circle arcs instead.
	corner_radius_kappa: num;

	border_color_left   : color;
	border_color_right  : color;
	border_color_top    : color;
	border_color_bottom : color;

	border_dash: arr(double);
	border_dash_offset: int;

	border_offset: num;

	_background_type: enum; --BACKGROUND_TYPE_*
	union {
		background_color: color;
		_background_linear_gradient: LinearGradient;
		_background_radial_gradient: RadialGradient;
		_background_image: Bitmap;
	}
	_background_pattern: &cairo_pattern_t;

	background_hittable: bool;
	-- overlapping between background clipping edge and border stroke.
	-- -1..1 goes from inside to outside of border edge.
	background_clip_border_offset: num;
	background_operator: enum; --OPERATOR_*

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

	--text --------------------------------------------------------------------

	text_runs: tr2.TextRuns;
	text_align_x: enum; --ALIGN_*
	text_align_y: enum; --ALIGN_*
	text_segments: tr2.Segs;

	caret: &CaretProperties;
	caret_insert_mode: bool;

	text_selectable: bool;
	text_selection: tr2.Selection;

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
		flex: FlexboxLayoutProperties;
		grid: GridLayoutProperties;
	}

	--flex layout / element fields
	fr: num;
	break_before: bool;
	break_after : bool;

	--grid layout / element fields
	grid_row: int;
	grid_col: int;
	grid_row_span: int;
	grid_col_span: int;
	--computed by the auto-positioning algorithm.
	_grid_row: int;
	_grid_col: int;
	_grid_row_span: int;
	_grid_col_span: int;

}

gettersandsetters(Layer)

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

--offset a rectangle by d (outward if d is positive)
local terra box2d_offset(d: num, x: num, y: num, w: num, h: num)
	return x - d, y - d, w + 2*d, h + 2*d
end

--layer hierarchy ------------------------------------------------------------

terra Layer:get_parent() --child interface
	return self._parent
end

terra Layer:_add_layer(e: &Layer)
	assert(self.children:add(@e) ~= -1)
	if e.parent ~= nil then
		e.parent:_remove_layer(e)
	end
	e._parent = self
end

terra Layer:set_parent(parent: &Layer)
	if parent ~= nil then
		parent:_add_layer(self)
	elseif self.parent ~= nil then
		self.parent:_remove_layer(self)
	end
end

terra Layer:_remove_layer(e: &Layer)
	self.children:remove(self.children:indexat(e))
end

terra Layer:_move_layer(i0: int, i1: int)
	i1 = clamp(i1, 0, self.children.len-1)
	if i0 == i1 then return end
	var e = self.children(i0)
	self.children:remove(i0)
	assert(self.children:insert(i1, e))
end

terra Layer:get_layer_index(): int
	return self.parent.children:indexat(self)
end

terra Layer:set_layer_index(i: int)
	self.parent:_move_layer(self.layer_index, i)
end

terra Layer:to_back()
	self.layer_index = 0
end

terra Layer:to_front()
	self.layer_index = maxint
end

--geometry utils -------------------------------------------------------------

terra Layer:get_px(): num return self.padding_left end
terra Layer:get_py(): num return self.padding_top end
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

--layer relative geometry & matrix -------------------------------------------

terra Layer:rel_matrix() --box matrix relative to parent's content space
	var m: matrix; m:init()
	m:translate(self:snapx(self.x), self:snapy(self.y))
	if self.rotation ~= 0 then
		m:rotate_around(self.rotation_cx, self.rotation_cy, rad(self.rotation))
	end
	if self.scale ~= 1 then
		m:scale_around(self.scale_cx, self.scale_cy, self.scale, self.scale)
	end
	return m
end

terra Layer:abs_matrix(): matrix --box matrix in window space
	var am: matrix
	if self.parent ~= nil then
		am = self.parent:abs_matrix()
	else
		am:init()
	end
	var rm = self:rel_matrix()
	am:transform(&rm)
	return am
end

terra Layer:cr_abs_matrix(cr: &cairo_t) --box matrix in cr's current space
	var cm = cr:matrix()
	var rm = self:rel_matrix()
	cm:transform(&rm)
	return cm
end

--convert point from own box space to parent content space.
terra Layer:from_box_to_parent(x: num, y: num)
	var m = self:rel_matrix()
	return m:point(x, y)
end

--convert point from parent content space to own box space.
terra Layer:from_parent_to_box(x: num, y: num)
	var m = self:rel_matrix(); m:invert()
	return m:point(x, y)
end

--convert point from own content space to parent content space.
terra Layer:to_parent(x: num, y: num)
	var m = self:rel_matrix()
	m:translate(self.px, self.py)
	return m:point(x, y)
end

--convert point from parent content space to own content space.
terra Layer:from_parent(x: num, y: num)
	var m = self:rel_matrix()
	m:translate(self.px, self.py)
	m:invert()
	return m:point(x, y)
end

terra Layer:to_window(x: num, y: num): {num, num} --parent & child interface
	var x, y = self:to_parent(x, y)
	if self.parent ~= nil then
		return self.parent:to_window(x, y)
	else
		return x, y
	end
end

terra Layer:from_window(x: num, y: num): {num, num} --parent & child interface
	if self.parent ~= nil then
		x, y = self.parent:from_window(x, y)
	end
	return self:from_parent(x, y)
end

--TODO: window connection
--[[
terra Layer:to_screen(x: num, y: num)
	var x, y = self:to_window(x, y)
	return self.window:to_screen(x, y)
end

terra Layer:from_screen(x: num, y: num)
	var x, y = self.window:from_screen(x, y)
	return self:from_window(x, y)
end

--convert point from own content space to other's content space.
terra Layer:to_other(other: &other, x: num, y: num)
	if other.window == self.window then
		x, y = self:to_window(x, y)
		return other:from_window(x, y)
	else
		x, y = self:to_screen(x, y)
		return other:from_screen(x, y)
	end
end

--convert point from other's content space to own content space
terra Layer:from_other(other: &Layer, x: num, y: num)
	return other:to_other(self, x, y)
end
]]

--border geometry and drawing ------------------------------------------------

--border edge widths relative to box rect at %-offset in border width.
--offset is in -1..1 where -1=inner edge, 0=center, 1=outer edge.
--returned widths are positive when inside and negative when outside box rect.
terra Layer:_border_edge_widths(offset: num)
	var o = self.border_offset + offset + 1
	var w1 = lerp(o, -1, 1, self.border_width_left,   0)
	var h1 = lerp(o, -1, 1, self.border_width_top,    0)
	var w2 = lerp(o, -1, 1, self.border_width_right,  0)
	var h2 = lerp(o, -1, 1, self.border_width_bottom, 0)
	--adjust overlapping widths by scaling them down proportionally.
	if w1 + w2 > self.w or h1 + h2 > self.h then
		var scale = min(self.w / (w1 + w2), self.h / (h1 + h2))
		w1 = w1 * scale
		h1 = h1 * scale
		w2 = w2 * scale
		h2 = h2 * scale
	end
	return w1, h1, w2, h2
end

terra Layer:border_pos(offset: num)
	var w, h, _, _2 = self:_border_edge_widths(offset)
	return w, h
end

--border rect at %-offset in border width.
terra Layer:border_rect(offset: num, size_offset: num)
	var w1, h1, w2, h2 = self:_border_edge_widths(offset)
	var w = self.w - w2 - w1
	var h = self.h - h2 - h1
	return box2d_offset(size_offset, w1, h1, w, h)
end

--corner radius at pixel offset from the stroke's center on one dimension.
local terra offset_radius(r: num, o: num)
	return iif(r > 0, max(0, r + o), 0)
end

--border rect at %-offset in border width, plus radii of rounded corners.
terra Layer:border_round_rect(offset: num, size_offset: num)

	var k = self.corner_radius_kappa

	var x1, y1, w, h = self:border_rect(0, 0) --at stroke center
	var X1, Y1, W, H = self:border_rect(offset, size_offset) --at offset

	var x2, y2 = x1 + w, y1 + h
	var X2, Y2 = X1 + W, Y1 + H

	var r1 = self.corner_radius_top_left
	var r2 = self.corner_radius_top_right
	var r3 = self.corner_radius_bottom_right
	var r4 = self.corner_radius_bottom_left

	--offset the radii to preserve curvature at offset.
	var r1x = offset_radius(r1, x1-X1)
	var r1y = offset_radius(r1, y1-Y1)
	var r2x = offset_radius(r2, X2-x2)
	var r2y = offset_radius(r2, y1-Y1)
	var r3x = offset_radius(r3, X2-x2)
	var r3y = offset_radius(r3, Y2-y2)
	var r4x = offset_radius(r4, x1-X1)
	var r4y = offset_radius(r4, Y2-y2)

	--remove degenerate arcs.
	if r1x == 0 or r1y == 0 then r1x = 0; r1y = 0 end
	if r2x == 0 or r2y == 0 then r2x = 0; r2y = 0 end
	if r3x == 0 or r3y == 0 then r3x = 0; r3y = 0 end
	if r4x == 0 or r4y == 0 then r4x = 0; r4y = 0 end

	--adjust overlapping radii by scaling them down proportionally.
	var maxx = max(r1x + r2x, r3x + r4x)
	var maxy = max(r1y + r4y, r2y + r3y)
	if maxx > W or maxy > H then
		var scale = min(W / maxx, H / maxy)
		r1x = r1x * scale
		r1y = r1y * scale
		r2x = r2x * scale
		r2y = r2y * scale
		r3x = r3x * scale
		r3y = r3y * scale
		r4x = r4x * scale
		r4y = r4y * scale
	end

	return
		X1, Y1, W, H,
		r1x, r1y, r2x, r2y, r3x, r3y, r4x, r4y,
		k
end

--De Casteljau split of a cubic bezier at time t (from path2d).
local terra bezier_split(first: bool, t: num,
	x1: num, y1: num, x2: num, y2: num, x3: num, y3: num, x4: num, y4: num
)
	var mt = 1-t
	var x12 = x1 * mt + x2 * t
	var y12 = y1 * mt + y2 * t
	var x23 = x2 * mt + x3 * t
	var y23 = y2 * mt + y3 * t
	var x34 = x3 * mt + x4 * t
	var y34 = y3 * mt + y4 * t
	var x123 = x12 * mt + x23 * t
	var y123 = y12 * mt + y23 * t
	var x234 = x23 * mt + x34 * t
	var y234 = y23 * mt + y34 * t
	var x1234 = x123 * mt + x234 * t
	var y1234 = y123 * mt + y234 * t
	if first then
		return x1, y1, x12, y12, x123, y123, x1234, y1234 --first curve
	else
		return x1234, y1234, x234, y234, x34, y34, x4, y4 --second curve
	end
end

local kappa = 4 / 3 * (sqrt(2) - 1)

--more-aesthetically-pleasing elliptic arc. only for 45deg and 90deg sweeps!
local terra bezier_qarc(cr: &cairo_t, cx: num, cy: num, rx: num, ry: num, q1: num, qlen: num, k: num)
	cr:save()
	cr:translate(cx, cy)
	cr:scale(rx / ry, 1)
	cr:rotate(floor(min(q1, q1 + qlen) - 2) * PI / 2)
	var r = ry
	var k = r * kappa * k
	var x1, y1, x2, y2, x3, y3, x4, y4 = 0, -r, k, -r, r, -k, r, 0
	if qlen < 0 then --reverse curve
		x1, y1, x2, y2, x3, y3, x4, y4 = x4, y4, x3, y3, x2, y2, x1, y1
		qlen = abs(qlen)
	end
	if qlen ~= 1 then
		assert(qlen == .5)
		var first = q1 == floor(q1)
		x1, y1, x2, y2, x3, y3, x4, y4 =
			bezier_split(first, qlen, x1, y1, x2, y2, x3, y3, x4, y4)
	end
	cr:line_to(x1, y1)
	cr:curve_to(x2, y2, x3, y3, x4, y4)
	cr:restore()
end

--draw a rounded corner: q1 is the quadrant starting top-left going clockwise.
--qlen is in 90deg units and can only be +/- .5 or 1 if k ~= 1.
terra Layer:corner_path(cr: &cairo_t, cx: num, cy: num, rx: num, ry: num, q1: num, qlen: num, k: num)
	if rx == 0 or ry == 0 then --null arcs need a line to the first endpoint
		assert(rx == 0 and ry == 0)
		cr:line_to(cx, cy)
	elseif k == 1 then --geometrically-correct elliptic arc
		var q2 = q1 + qlen
		var a1 = (q1 - 3) * PI / 2
		var a2 = (q2 - 3) * PI / 2
		if a1 < a2 then
			cr:elliptic_arc(cx, cy, rx, ry, 0, a1, a2)
		else
			cr:elliptic_arc_negative(cx, cy, rx, ry, 0, a1, a2)
		end
	else
		bezier_qarc(cr, cx, cy, rx, ry, q1, qlen, k)
	end
end

terra Layer:border_line_to(cr: &cairo_t, x: num, y: num, q: num) end --stub (used by tablist)

--trace the border contour path at offset.
--offset is in -1..1 where -1=inner edge, 0=center, 1=outer edge.
terra Layer:border_path(cr: &cairo_t, offset: num, size_offset: num)
	var x1, y1, w, h, r1x, r1y, r2x, r2y, r3x, r3y, r4x, r4y, k =
		self:border_round_rect(offset, size_offset)
	var x2, y2 = x1 + w, y1 + h
	cr:move_to(x1, y1+r1y)
	self:corner_path    (cr, x1+r1x, y1+r1y, r1x, r1y, 1, 1, k) --tl
	self:border_line_to (cr, x2-r2x, y1, 1)
	self:corner_path    (cr, x2-r2x, y1+r2y, r2x, r2y, 2, 1, k) --tr
	self:border_line_to (cr, x2, y2-r3y, 2)
	self:corner_path    (cr, x2-r3x, y2-r3y, r3x, r3y, 3, 1, k) --br
	self:border_line_to (cr, x1+r4x, y2, 3)
	self:corner_path    (cr, x1+r4x, y2-r4y, r4x, r4y, 4, 1, k) --bl
	self:border_line_to (cr, x1, y1+r1y, 4)
	cr:close_path()
end

terra Layer:border_visible(): bool
	return
		   self.border_width_left   ~= 0
		or self.border_width_top    ~= 0
		or self.border_width_right  ~= 0
		or self.border_width_bottom ~= 0
end

terra Layer:draw_border(cr: &cairo_t)
	if not self:border_visible() then return end

	--seamless drawing when all side colors are the same.
	if self.border_color_left == self.border_color_top
		and self.border_color_left == self.border_color_right
		and self.border_color_left == self.border_color_bottom
	then
		cr:new_path()
		cr:rgba(self.border_color_bottom)
		if self.border_width_left == self.border_width_top
			and self.border_width_left == self.border_width_right
			and self.border_width_left == self.border_width_bottom
		then --stroke-based terra (doesn't require path offseting; supports dashing)
			self:border_path(cr, 0, 0)
			cr:line_width(self.border_width_left)
			if self.border_dash.len > 0 then
				cr:dash(self.border_dash.elements, self.border_dash.len, self.border_dash_offset)
			end
			cr:stroke()
		else --fill-based terra (requires path offsetting; supports patterns)
			cr:fill_rule(CAIRO_FILL_RULE_EVEN_ODD)
			self:border_path(cr, -1, 0)
			self:border_path(cr,  1, 0)
			cr:fill()
		end
		return
	end

	--complicated drawing of each side separately.
	--still shows seams on adjacent sides of the same color.
	var x1, y1, w, h, r1x, r1y, r2x, r2y, r3x, r3y, r4x, r4y, k =
		self:border_round_rect(-1, 0)
	var X1, Y1, W, H, R1X, R1Y, R2X, R2Y, R3X, R3Y, R4X, R4Y, K =
		self:border_round_rect( 1, 0)

	var x2, y2 = x1 + w, y1 + h
	var X2, Y2 = X1 + W, Y1 + H

	if self.border_color_left.alpha > 0 then
		cr:new_path()
		cr:move_to(x1, y1+r1y)
		self:corner_path(cr, x1+r1x, y1+r1y, r1x, r1y, 1, .5, k)
		self:corner_path(cr, X1+R1X, Y1+R1Y, R1X, R1Y, 1.5, -.5, K)
		cr:line_to(X1, Y2-R4Y)
		self:corner_path(cr, X1+R4X, Y2-R4Y, R4X, R4Y, 5, -.5, K)
		self:corner_path(cr, x1+r4x, y2-r4y, r4x, r4y, 4.5, .5, k)
		cr:close_path()
		cr:rgba(self.border_color_left)
		cr:fill()
	end

	if self.border_color_top.alpha > 0 then
		cr:new_path()
		cr:move_to(x2-r2x, y1)
		self:corner_path(cr, x2-r2x, y1+r2y, r2x, r2y, 2, .5, k)
		self:corner_path(cr, X2-R2X, Y1+R2Y, R2X, R2Y, 2.5, -.5, K)
		cr:line_to(X1+R1X, Y1)
		self:corner_path(cr, X1+R1X, Y1+R1Y, R1X, R1Y, 2, -.5, K)
		self:corner_path(cr, x1+r1x, y1+r1y, r1x, r1y, 1.5, .5, k)
		cr:close_path()
		cr:rgba(self.border_color_top)
		cr:fill()
	end

	if self.border_color_right.alpha > 0 then
		cr:new_path()
		cr:move_to(x2, y2-r3y)
		self:corner_path(cr, x2-r3x, y2-r3y, r3x, r3y, 3, .5, k)
		self:corner_path(cr, X2-R3X, Y2-R3Y, R3X, R3Y, 3.5, -.5, K)
		cr:line_to(X2, Y1+R2Y)
		self:corner_path(cr, X2-R2X, Y1+R2Y, R2X, R2Y, 3, -.5, K)
		self:corner_path(cr, x2-r2x, y1+r2y, r2x, r2y, 2.5, .5, k)
		cr:close_path()
		cr:rgba(self.border_color_right)
		cr:fill()
	end

	if self.border_color_bottom.alpha > 0 then
		cr:new_path()
		cr:move_to(x1+r4x, y2)
		self:corner_path(cr, x1+r4x, y2-r4y, r4x, r4y, 4, .5, k)
		self:corner_path(cr, X1+R4X, Y2-R4Y, R4X, R4Y, 4.5, -.5, K)
		cr:line_to(X2-R3X, Y2)
		self:corner_path(cr, X2-R3X, Y2-R3Y, R3X, R3Y, 4, -.5, K)
		self:corner_path(cr, x2-r3x, y2-r3y, r3x, r3y, 3.5, .5, k)
		cr:close_path()
		cr:rgba(self.border_color_bottom)
		cr:fill()
	end
end

--background geometry and drawing --------------------------------------------

BACKGROUND_TYPE_NONE            = 0
BACKGROUND_TYPE_COLOR           = 1
BACKGROUND_TYPE_LINEAR_GRADIENT = 2
BACKGROUND_TYPE_RADIAL_GRADIENT = 3
BACKGROUND_TYPE_IMAGE           = 4

BACKGROUND_EXTEND_NO      = 0
BACKGROUND_EXTEND_REPEAT  = 1
BACKGROUND_EXTEND_REFLECT = 2

terra Layer:get_background_type()
	return self._background_type
end

terra Layer:set_background_type(type: enum)
	if self._background_type == type then return end
	if self._background_pattern ~= nil then
		self._background_pattern:free()
		self._background_pattern = nil
	end
	if type == BACKGROUND_TYPE_COLOR then
		self.background_color = color {0, 0, 0, 0}
	elseif type == BACKGROUND_TYPE_LINEAR_GRADIENT then
		fill(&self._background_linear_gradient)
	elseif type == BACKGROUND_TYPE_RADIAL_GRADIENT then
		fill(&self._background_radial_gradient)
	elseif type == BACKGROUND_TYPE_IMAGE then
		fill(&self._background_image)
	end
end

terra Layer:background_visible(): bool
	return self.background_type ~= BACKGROUND_TYPE_NONE
end

terra Layer:background_rect(size_offset: num)
	return self:border_rect(self.background_clip_border_offset, size_offset)
end

terra Layer:background_round_rect(size_offset: num)
	return self:border_round_rect(self.background_clip_border_offset, size_offset)
end

terra Layer:background_path(cr: &cairo_t, size_offset: num)
	self:border_path(cr, self.background_clip_border_offset, size_offset)
end

terra Layer:get_background_linear_gradient()
	return self._background_linear_gradient
end

terra Layer:set_background_linear_gradient(g: LinearGradient)
	self.background_type = BACKGROUND_TYPE_LINEAR_GRADIENT
	self._background_linear_gradient = g
	if self._background_pattern ~= nil then
		self._background_pattern:free()
	end
	self._background_pattern = cairo_pattern_create_linear(g.x1, g.y1, g.x2, g.y2)
	for _,c in g.color_stops do
		self._background_pattern:add_color_stop_rgba(c.offset, unpacktuple(c.color))
	end
end

terra Layer:get_background_radial_gradient()
	return self._background_radial_gradient
end

terra Layer:set_background_radial_gradient(g: RadialGradient)
	self.background_type = BACKGROUND_TYPE_RADIAL_GRADIENT
	self._background_radial_gradient = g
	if self._background_pattern ~= nil then
		self._background_pattern:free()
	end
	self._background_pattern = cairo_pattern_create_radial(
		g.cx1, g.cy1, g.r1, g.cx2, g.cy2, g.r2)
	for _,c in g.color_stops do
		self._background_pattern:add_color_stop_rgba(c.offset, unpacktuple(c.color))
	end
end

terra Bitmap:surface()
	return cairo_image_surface_create_for_data(
		[&uint8](self.pixels), self.format, self.w, self.h, self.stride)
end

terra Layer:get_background_image()
	return self._background_image
end

terra Layer:set_background_image(b: Bitmap)
	self.background_type = BACKGROUND_TYPE_IMAGE
	self._background_image = b
	if self._background_pattern ~= nil then
		self._background_pattern:free()
	end
	self._background_pattern = cairo_pattern_create_for_surface(b:surface())
end

terra Layer:paint_background(cr: &cairo_t)
	cr:operator(self.background_operator)
	if self.background_type == BACKGROUND_TYPE_COLOR then
		cr:rgba(self.background_color)
		cr:paint()
		return
	end
	var m: matrix; m:init()
	m:translate(
		self.background_x,
		self.background_y)
	if self.background_rotation ~= 0 then
		m:rotate_around(
			self.background_rotation_cx,
			self.background_rotation_cy,
			rad(self.background_rotation))
	end
	if self.background_scale ~= 1 then
		m:scale_around(
			self.background_scale_cx,
			self.background_scale_cy,
			self.background_scale,
			self.background_scale)
	end
	m:invert()
	var patt = self._background_pattern
	patt:matrix(&m)
	patt:extend(self.background_extend)
	cr:source(patt)
	cr:paint()
	cr:rgb(0, 0, 0) --release source
end

--content-box geometry, drawing and hit testing ------------------------------

--convert point from own box space to own content space.
terra Layer:to_content(x: num, y: num)
	return x - self.px, y - self.py
end

--content point from own content space to own box space.
terra Layer:from_content(x: num, y: num)
	return self.px + x, self.py + y
end

--children geometry, drawing and hit testing ---------------------------------

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

terra Layer:_draw_children(cr: &cairo_t): {} --called in content space
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

--text geometry & drawing ----------------------------------------------------

terra Layer:text_visible()
	return self.text_runs.array.len > 0
end

terra Layer:_sync_text_shape()
	if not self:text_visible() then return false end
	self._manager.tr:shape(&self.text_runs, &self.text_segments)
	return true
end

terra Layer:_sync_text_wrap()
	self.text_segments:wrap(self.cw)
end

terra Layer:_sync_text_align()
	self.text_segments:align(0, 0, self.cw, self.ch,
		self.text_align_x, self.text_align_y)
	if self.text_selectable then
		self.text_selection:init(&self.text_segments)
	end
end

terra Layer:get_baseline()
	if not self:text_visible() then return self.h end
	return self.text_segments.lines.baseline
end

terra Layer:_draw_text(cr: &cairo_t)
	var x1: double, y1: double, x2: double, y2: double
	cr:clip_extents(&x1, &y1, &x2, &y2)
	self.text_segments:clip(x1, y1, x2-x1, y2-y1)
	self._manager.tr:paint(cr, &self.text_segments)
end

--[[
function layer:text_bbox()
	if not self:text_visible() then
		return 0, 0, 0, 0
	end
	return self.text_segments:bbox()
end
]]

--text caret & selection drawing ---------------------------------------------

--[[
terra Layer:caret_rect()
	local x, y, w, h = self.text_selection.cursor2:rect(self.caret_width)
	local x, w = self:snapxw(x, w)
	local y, h = self:snapyh(y, h)
	return x, y, w, h
end

terra Layer:caret_visibility_rect()
	local x, y, w, h = self:caret_rect()
	--enlarge the caret rect to contain the line spacing.
	local line = self.text_selection.cursor2.seg.line
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
	local sel = self.text_selection
	if not sel then return end
	if sel:empty() then return end
	cr:rgba(self.ui:rgba(self.text_selection_color))
	cr:new_path()
	sel:rectangles(self.draw_selection_rect, self, cr)
end

terra Layer:make_visible_caret()
	local segs = self.text_segments
	local lines = segs.lines
	local sx, sy = lines.x, lines.y
	local cw, ch = self:client_size()
	local x, y, w, h = self:caret_visibility_rect()
	lines.x, lines.y = box2d.scroll_to_view(x-sx, y-sy, w, h, cw, ch, sx, sy)
	self:make_visible(self:caret_visibility_rect())
end
]]

terra Layer:_draw_text_selection(cr: &cairo_t) end
terra Layer:_draw_caret(cr: &cairo_t) end

--drawing & hit testing ------------------------------------------------------

terra Layer:draw_content(cr: &cairo_t) --called in own content space
	self:_draw_children(cr)
	self:_draw_text_selection(cr)
	self:_draw_text(cr)
	self:_draw_caret(cr)
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
	var bg = self:background_visible()

	--TODO: self:draw_shadow(cr)

	var clip = bg or cc
	if clip then
		cr:save()
		cr:new_path()
		self:background_path(cr, 0) --'background' clipping is implicit here
		cr:clip()
		if bg then
			self:paint_background(cr)
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

--layout plugin interface ----------------------------------------------------

LAYOUT_NULL    = 0
LAYOUT_TEXTBOX = 1
LAYOUT_FLEXBOX = 2
LAYOUT_GRID    = 3

--layout interface forwarders
terra Layer:_sync_layout()          return self._layout.sync(self) end
terra Layer:_sync_min_w(b: bool)    return self._layout.sync_min_w(self, b) end
terra Layer:_sync_min_h(b: bool)    return self._layout.sync_min_h(self, b) end
terra Layer:_sync_layout_x(b: bool) return self._layout.sync_x(self, b) end
terra Layer:_sync_layout_y(b: bool) return self._layout.sync_y(self, b) end

--layouting constants --------------------------------------------------------

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

--layout utils ---------------------------------------------------------------

AXIS_ORDER_XY = 1
AXIS_ORDER_YX = 2

--used by layers that need to solve their layout on one axis completely
--before they can solve it on the other axis. any content-based layout with
--wrapped content is like that: can't know the height until wrapping the
--content which needs to know the width (and viceversa for vertical flow).
terra Layer:_sync_layout_separate_axes(axis_order: enum, min_w: num, min_h: num)
	if not self.visible then return end
	axis_order = iif(axis_order ~= 0, axis_order, self._layout.axis_order)
	var sync_x = axis_order == AXIS_ORDER_XY
	var axis_synced = false
	var other_axis_synced = false
	for phase = 0, 3 do
		other_axis_synced = axis_synced
		if sync_x then
			--sync the x-axis.
			self.w = max(self:_sync_min_w(other_axis_synced), min_w)
			axis_synced = self:_sync_layout_x(other_axis_synced)
		else
			--sync the y-axis.
			self.h = max(self:_sync_min_h(other_axis_synced), min_h)
			axis_synced = self:_sync_layout_y(other_axis_synced)
		end
		if axis_synced and other_axis_synced then
			break --both axes were solved before last phase.
		end
		sync_x = not sync_x
	end
	assert(axis_synced and other_axis_synced)
end

terra Layer:_sync_layout_children()
	for _,layer in self.children do
		layer:_sync_layout() --recurse
	end
end

--null layout ----------------------------------------------------------------

--layouting system entry point: called on the top layer.
--called by null-layout layers to layout themselves and their children.
local terra sync(self: &Layer)
	if not self.visible then return end
	self.x, self.w = self:snapxw(self.x, self.w)
	self.y, self.h = self:snapyh(self.y, self.h)
	if self:_sync_text_shape() then
		self:_sync_text_wrap()
		self:_sync_text_align()
	end
	self:_sync_layout_children()
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
		self:_sync_layout()
	end
	return true
end

local null_layout = constant(`Layout {
	type       = LAYOUT_NULL;
	axis_order = 0;
	init       = nil;
	free       = nil;
	sync       = sync;
	sync_min_w = sync_min_w;
	sync_min_h = sync_min_h;
	sync_x     = sync_x;
	sync_y     = sync_x;
})

--textbox layout -------------------------------------------------------------

local terra sync(self: &Layer)
	if not self.visible then return end
	if self:_sync_text_shape() then
		self.cw = 0
		self.ch = 0
		return
	end
	self.cw = max(self.text_segments:min_w(), self.min_cw)
	self:_sync_text_wrap()
	self.cw = max(self.text_segments.lines.max_ax, self.min_cw)
	self.ch = max(self.min_ch, self.text_segments.lines.spaced_h)
	self.x, self.w = self:snapxw(self.x, self.w)
	self.y, self.h = self:snapyh(self.y, self.h)
	self:_sync_text_align()
	self:_sync_layout_children()
end

local terra sync_min_w(self: &Layer, other_axis_synced: bool)
	var min_cw: num
	if not other_axis_synced then --TODO: or self.nowrap
		min_cw = iif(self:_sync_text_shape(), self.text_segments:min_w(), 0)
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
		self:_sync_text_wrap()
		return true
	end
end

local terra sync_y(self: &Layer, other_axis_synced: bool)
	if other_axis_synced then
		self:_sync_text_align()
		self:_sync_layout_children()
		return true
	end
end

local textbox_layout = constant(`Layout {
	type       = LAYOUT_TEXTBOX;
	axis_order = 0;
	init       = nil;
	free       = nil;
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

local function stretch_items_main_axis_func(T, X, W)

	local _MIN_W = '_min_'..W
	local ALIGN_X = 'align_'..X

	--compute a single item's stretched width and aligned width.
	local terra stretched_item_widths(item: &T, total_w: num,
		total_fr: num, total_overflow_w: num, total_free_w: num, align: enum
	)
		var min_w = item.[_MIN_W]
		var flex_w = total_w * item.fr / total_fr
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
		items: arr(T), i: int, j: int,
		sx: num, spacing: num,
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

	local terra items_sum_x(items: arr(Layer), i: int, j: int)
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

	local terra items_max_x(items: arr(Layer), i: int, j: int)
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

	Layer.methods['_flexbox_min_cw_'..X] = terra(
		self: &Layer, other_axis_synced: bool, align_baseline: bool
	)
		if self.flex.wrap then
			return items_max_x(self.children, 0, self.children.len)._0
		else
			return items_sum_x(self.children, 0, self.children.len)._0
		end
	end

	Layer.methods['_flexbox_min_ch_'..X] = terra(
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
			var sx: num, spacing: num
			if align == ALIGN_START or align == ALIGN_LEFT then
				sx, spacing = 0, 0
			else
				var items_w, item_count = items_sum_x(self.children, i, j)
				sx, spacing = align_metrics(align, self.[CW], items_w, item_count)
			end
			align_items_main_axis_x(
				self.children, i, j, sx, spacing, moving)
				--TODO: set_item_x, set_moving_item_x)
		end
	end

	--stretch or align a flexbox's items on the main-axis.
	Layer.methods['_flexbox_sync_x_'..X] = terra(
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
	Layer.methods['_flexbox_sync_y_'..X] = terra(
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
			layer:_sync_min_w(other_axis_synced) --recurse
		end
	end

	var min_cw = iif(self.flex.flow == FLEX_FLOW_X,
			self:_flexbox_min_cw_x(other_axis_synced, false),
			self:_flexbox_min_ch_y(other_axis_synced, false))

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
			var item_h = layer:_sync_min_h(other_axis_synced) --recurse
			--for baseline align also layout the children because we need
			--their baseline. we can do this here because we already know
			--we won't stretch them beyond their min_h in this case.
			if align_baseline then
				layer.h = snap(item_h, self.snap_y)
				layer:_sync_layout_y(other_axis_synced)
			end
		end
	end

	var min_ch = iif(self.flex.flow == FLEX_FLOW_X,
		self:_flexbox_min_ch_x(other_axis_synced, align_baseline),
		self:_flexbox_min_cw_y(other_axis_synced, align_baseline))

	min_ch = max(min_ch, self.min_ch)
	var min_h = min_ch + self.ph
	self._min_h = min_h
	return min_h
end

local terra sync_x(self: &Layer, other_axis_synced: bool)

	var synced = iif(self.flex.flow == FLEX_FLOW_X,
			self:_flexbox_sync_x_x(other_axis_synced, false),
			self:_flexbox_sync_y_y(other_axis_synced, false))

	if synced then
		--sync all children last (top-down sync).
		for _,layer in self.children do
			if layer.visible then
				layer:_sync_layout_x(other_axis_synced) --recurse
			end
		end
	end
	return synced
end

local terra sync_y(self: &Layer, other_axis_synced: bool)

	if self.flex.flow == FLEX_FLOW_X and self.item_align_y == ALIGN_BASELINE then
		--chilren already sync'ed in sync_min_h().
		return self:_flexbox_sync_y_x(other_axis_synced, true)
	end

	var synced = self.flex.flow == FLEX_FLOW_Y
		and self:_flexbox_sync_x_y(other_axis_synced, false)
		 or self:_flexbox_sync_y_x(other_axis_synced, false)

	if synced then
		--sync all children last (top-down sync).
		for _,layer in self.children do
			if layer.visible then
				layer:_sync_layout_y(other_axis_synced) --recurse
			end
		end
	end
	return synced
end

local flexbox_layout = constant(`Layout {
	type       = LAYOUT_FLEXBOX;
	axis_order = AXIS_ORDER_XY;
	init       = nil;
	free       = nil;
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

--bitmap-of-bools object -----------------------------------------------------

terra BoolBitmap:init()
	self.rows = 0
	self.cols = 0
	self.bits:init()
end

terra BoolBitmap:free()
	self.bits:free()
end

terra BoolBitmap:set(row: int, col: int, val: bool)
	self.bits:set((row - 1) * self.cols + col - 1, val)
end

terra BoolBitmap:get(row: int, col: int)
	return self.bits((row - 1) * self.cols + col, false)
end

terra BoolBitmap:widen(min_rows: int, min_cols: int)
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

terra BoolBitmap:mark(row1: int, col1: int, row_span: int, col_span: int, val: bool)
	var row2 = row1 + row_span
	var col2 = col1 + col_span
	self:widen(row2-1, col2-1)
	for row = row1, row2 do
		for col = col1, col2 do
			self:set(row, col, val)
		end
	end
end

terra BoolBitmap:check(row1: int, col1: int, row_span: int, col_span: int)
	var row2 = row1 + row_span
	var col2 = col1 + col_span
	for row = row1, row2 do
		for col = col1, col2 do
			if self:get(row, col) then
				return true
			end
		end
	end
	return false
end

terra BoolBitmap:clear()
	self:mark(1, 1, self.rows, self.cols, false)
end

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

terra Layer:_sync_layout_grid_autopos()

	var flow = self.grid.flow
	var col_first = (flow and GRID_FLOW_Y) == 0
	var row_first = not col_first
	var flip_cols = (flow and GRID_FLOW_R) ~= 0
	var flip_rows = (flow and GRID_FLOW_B) ~= 0
	var grid_wrap = max(1, self.grid.wrap)
	var max_col = iif(col_first, grid_wrap, self.grid.min_lines)
	var max_row = iif(row_first, grid_wrap, self.grid.min_lines)

	var occupied = self._manager.grid_occupied
	occupied:clear()

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
						clip_span(row, col, row_span, col_span, maxint, maxint)

					occupied:mark(row, col, row_span, col_span, true)

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

					occupied:mark(row, col, row_span, col_span, true)

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

				occupied:mark(row, col, row_span, col_span, true)

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

local stretch_cols_main_axis = stretch_items_main_axis_func(GridLayoutCol, 'x', 'w')
local align_cols_main_axis = align_items_main_axis_func(GridLayoutCol, 'w')

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
				layer:['_sync_min_'..W](other_axis_synced) --recurse
			end
		end

		var gap_w = self.grid.[COL_GAP]
		var max_col = self.grid.[_MAX_COL]
		var fr = self.grid.[COLS] --{fr1, ...}

		--compute the fraction representing the total width.
		var total_fr: num = 0
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
			cols:set(col, GridLayoutCol{
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
						var col_min_w =
							fr(col, 1) / span_fr * span_min_w
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

	local terra sum_min_w(cols: arr(GridLayoutCol))
		var w: num = 0
		for _,col in cols do
			w = w + col._min_w
		end
		return w
	end

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
			stretch_cols_main_axis(
				cols, 0, cols.len, container_w, ALIGN_STRETCH, false)
				--set_item_x, set_moving_item_x,
				--X, W, ALIGN_END, ALIGN_RIGHT)
		else
			var sx: num, spacing: num
			if align_items_x == ALIGN_START or align_items_x == ALIGN_LEFT then
				sx, spacing = 0, 0
			else
				var items_w = sum_min_w(cols)
				var items_count = cols.len
				sx, spacing = align_metrics(align_items_x, self.[CW], items_w, items_count)
			end
			align_cols_main_axis(cols, 0, cols.len, sx, spacing, false)
				--TODO: set_item_x, set_moving_item_x
		end

		var x: num = 0
		for _,layer in self.children do
			if layer.inlayout then

				var col1 = layer.[_COL]
				var col2 = col1 + layer.[_COL_SPAN] - 1
				var col_item1 = cols:at(col1)
				var col_item2 = cols:at(col2)
				var x1 = col_item1.x
				var x2 = col_item2.x + col_item2.w

				var gap1: num = iif(col1 ~= 1,        gap_w * .5, 0.0)
				var gap2: num = iif(col2 ~= cols.len, gap_w * .5, 0.0)
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
				layer:['_sync_layout_'..X](other_axis_synced) --recurse
			end
		end
		return true
	end

	return sync_min_w, sync_x
end
local sync_min_w, sync_x = gen_funcs('x', 'y', 'w', 'h', 'col', ALIGN_LEFT, ALIGN_RIGHT)
local sync_min_h, sync_y = gen_funcs('y', 'x', 'h', 'w', 'row', ALIGN_TOP, ALIGN_BOTTOM)

local terra sync(self: &Layer)
	self:_sync_layout_grid_autopos()
	self:_sync_layout_separate_axes(0, -inf, -inf)
end

local terra init(self: &Layer)
	self.grid.cols:init()
	self.grid.rows:init()
	self.grid._cols:init()
	self.grid._rows:init()
end

local terra free(self: &Layer)
	self.grid.cols:free()
	self.grid.rows:free()
	self.grid._cols:free()
	self.grid._rows:free()
end

local grid_layout = constant(`Layout {
	type       = LAYOUT_GRID;
	axis_order = AXIS_ORDER_XY;
	init       = init;
	free       = free;
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
	if self._layout.free ~= nil then
		self._layout.free(self)
	end
	self._layout = layouts[type]
	if self._layout.init ~= nil then
		self._layout.init(self)
	end
end

--init/free ------------------------------------------------------------------

terra Layer:init(manager: &LayerManager)

	fill(self)

	self._manager = manager
	self.children:init()

	self.scale = 1.0
	self.snap_x = true
	self.snap_y = true

	self.border_dash:init()
	self.border_offset = -1  --inner border
	self.corner_radius_kappa = 1.2

	self.background_hittable = true
	self.background_scale = 1.0
	self.background_extend = BACKGROUND_EXTEND_REPEAT
	self.background_operator = CAIRO_OPERATOR_OVER
	self.background_clip_border_offset = 1

	self.text_align_x = ALIGN_CENTER
	self.text_align_y = ALIGN_CENTER
	self.caret = &self._manager.caret

	self.opacity = 1

	self._layout = null_layout
	self.align_items_x = ALIGN_STRETCH
	self.align_items_y = ALIGN_STRETCH
	self.item_align_x  = ALIGN_STRETCH
	self.item_align_y  = ALIGN_STRETCH

end

terra Layer:free(): {}
	self.children:free()
	self.border_dash:free()
	self.layout = LAYOUT_NULL
	self._manager:free_layer(self)
end

--layer manager --------------------------------------------------------------

terra LayerManager:init()
	self.tr:init()
	self.grid_occupied:init()
	self.layers:init()

	self.caret.width = 1
	self.caret.color = color {1, 1, 1, 1}
end

terra LayerManager:free()
	self.layers:free()
	self.grid_occupied:free()
	self.tr:free()
end

terra LayerManager:new_layer()
	var layer = self.layers:alloc()
	assert(layer ~= nil)
	layer:init(self)
	return layer
end

terra LayerManager:free_layer(layer: &Layer)
	self.layers:release(layer)
end

--C/ffi module ---------------------------------------------------------------

function compile_module()
	low.compile_module{
		name = 'layer',
		types = {Layer, LayerManager},
		publish = function(type, name)
			return not (
				   name:starts'get_'
				or name:starts'_'
				or name:starts'snap'
			)
		end,
		linkto = {'cairo', 'freetype', 'harfbuzz', 'fribidi', 'unibreak'},
	}
end

compile_module()

return _M
