--go@ luajit -jp=z *
local layerlib = {__index = require'low'}
setfenv(1, setmetatable(layerlib, layerlib))
require'cairolib'
struct LayerText;
local tr = require'trlib_env'
struct tr.TextRunState { t: &LayerText; }
require'trlib_paint_cairo'
local tr = require'trlib'
local boxblur = require'boxblurlib'
local bitmap = require'bitmaplib'

ALIGN_AUTO          = tr.ALIGN_AUTO    --only for align_x
ALIGN_LEFT          = tr.ALIGN_LEFT
ALIGN_RIGHT         = tr.ALIGN_RIGHT
ALIGN_CENTER        = tr.ALIGN_CENTER
ALIGN_TOP           = tr.ALIGN_TOP     --same as ALIGN_LEFT!
ALIGN_BOTTOM        = tr.ALIGN_BOTTOM  --same as ALIGN_RIGHT!
ALIGN_STRETCH       = tr.ALIGN_MAX + 1
ALIGN_START         = tr.ALIGN_MAX + 2 --left for LTR text, right for RTL
ALIGN_END           = tr.ALIGN_MAX + 3 --right for LTR text, left for RTL
ALIGN_SPACE_EVENLY  = tr.ALIGN_MAX + 4
ALIGN_SPACE_AROUND  = tr.ALIGN_MAX + 5
ALIGN_SPACE_BETWEEN = tr.ALIGN_MAX + 6
ALIGN_BASELINE      = tr.ALIGN_MAX + 7

--types ----------------------------------------------------------------------

color = cairo_argb32_color_t
matrix = cairo_matrix_t

Bitmap = bitmap.Bitmap;

terra Bitmap:surface()
	return cairo_image_surface_create_for_data(
		[&uint8](self.pixels), self.format, self.w, self.h, self.stride)
end

struct BoolBitmap {
	rows: int;
	cols: int;
	bits: arr(bool);
}

terra BoolBitmap:init()
	self.rows = 0
	self.cols = 0
	self.bits:init()
end

terra BoolBitmap:free()
	self.bits:free()
end

struct Layer;

BorderLineToFunc = {&Layer, &cairo_t, num, num, num} -> {}
BorderLineToFunc.__typename_ffi = 'BorderLineToFunc'

struct LayerBorder (gettersandsetters) {
	left   : num;
	right  : num;
	top    : num;
	bottom : num;

	corner_radius_top_left     : num;
	corner_radius_top_right    : num;
	corner_radius_bottom_left  : num;
	corner_radius_bottom_right : num;
	--draw rounded corners with a modified bezier for smoother line-to-arc
	--transitions. kappa=1 uses circle arcs instead.
	corner_radius_kappa: num;

	color_left   : color;
	color_right  : color;
	color_top    : color;
	color_bottom : color;

	dash: arr(double);
	dash_offset: int;

	offset: num;

	line_to: BorderLineToFunc;
}

local default_border = constant(`LayerBorder {
	left = 0;
	right = 0;
	top = 0;
	bottom = 0;

	corner_radius_top_left     = 0;
	corner_radius_top_right    = 0;
	corner_radius_bottom_left  = 0;
	corner_radius_bottom_right = 0;
	corner_radius_kappa = 1.2;

	color_left   = color {0};
	color_right  = color {0};
	color_top    = color {0};
	color_bottom = color {0};

	dash = [arr(double)](nil);
	dash_offset = 0;

	offset = -1;  --inner border

	line_to = nil;
})

terra LayerBorder:init()
	@self = default_border
end

terra LayerBorder:free()
	self.dash:free()
end

struct ColorStop {
	offset: num;
	color: color;
}

struct LinearGradientPoints {
	x1: num; y1: num;
	x2: num; y2: num;
}

struct RadialGradientCircles {
	cx1: num; cy1: num; r1: num;
	cx2: num; cy2: num; r2: num;
}

GRADIENT_TYPE_LINEAR = 1
GRADIENT_TYPE_RADIAL = 2

struct Gradient (gettersandsetters) {
	_type: enum; --GRADIENT_TYPE_*
	color_stops: arr(ColorStop);
	union {
		_points: LinearGradientPoints;
		_circles: RadialGradientCircles;
	}
}

terra Gradient:get_type()
	return self._type
end

terra Gradient:set_type(type: enum)
	if self._type ~= type then
		if type == GRADIENT_TYPE_LINEAR then
			fill(&self._points)
		elseif type == GRADIENT_TYPE_RADIAL then
			fill(&self._circles)
		end
		self._type = type
	end
end

terra Gradient:init()
	self.type = GRADIENT_TYPE_LINEAR
	self.color_stops:init()
end

terra Gradient:free()
	self.color_stops:free()
end

terra Gradient:get_points()
	assert(self.type == GRADIENT_TYPE_LINEAR)
	return &self._points
end

terra Gradient:get_circles()
	assert(self.type == GRADIENT_TYPE_RADIAL)
	return &self._circles
end

terra Gradient:get_newpoints()
	self.type = GRADIENT_TYPE_LINEAR
	return &self._points
end

terra Gradient:get_newcircles()
	self.type = GRADIENT_TYPE_RADIAL
	return &self._circles
end

BACKGROUND_TYPE_NONE     = 0
BACKGROUND_TYPE_COLOR    = 1
BACKGROUND_TYPE_GRADIENT = 2
BACKGROUND_TYPE_IMAGE    = 3

BACKGROUND_EXTEND_NO      = 0
BACKGROUND_EXTEND_REPEAT  = 1
BACKGROUND_EXTEND_REFLECT = 2

struct LayerBackground (gettersandsetters) {
	_type: enum; --BACKGROUND_TYPE_*
	union {
		_color: color;
		_gradient: Gradient;
		_image: Bitmap;
	}
	_pattern: &cairo_pattern_t;

	hittable: bool;
	-- overlapping between background clipping edge and border stroke.
	-- -1..1 goes from inside to outside of border edge.
	clip_border_offset: num;
	operator: enum; --OPERATOR_*

	x: num;
	y: num;

	rotation: num;
	rotation_cx: num;
	rotation_cy: num;

	scale: num;
	scale_cx: num;
	scale_cy: num;

	extend: enum; --BACKGROUND_EXTEND_*
}

local default_background = constant(`LayerBackground {
	_type = BACKGROUND_TYPE_NONE;
	_pattern = nil;

	hittable = true;

	clip_border_offset = 1;
	operator = CAIRO_OPERATOR_OVER;

	x = 0.0;
	y = 0.0;

	rotation = 0.0;
	rotation_cx = 0.0;
	rotation_cy = 0.0;

	scale = 1.0;
	scale_cx = 0.0;
	scale_cy = 0.0;

	extend = BACKGROUND_EXTEND_REPEAT;
})

struct ShadowState {
	blur: boxblur.Blur;
	blurred_surface: &cairo_surface_t;
	x: num; y: num;
}

struct LayerShadow {
	x: num;
	y: num;
	color: color;
	blur: uint8;
	passes: uint8;
	_state: ShadowState;
}

local default_shadow = constant(`LayerShadow {
	x = 0.0; y = 0.0; blur = 0; passes = 0;
	_state = ShadowState{blurred_surface = nil};
})

struct LayerFont (gettersandsetters) {
	f: tr.Font;
}

struct LayerText {
	runs: tr.TextRuns;
	align_x: enum; --ALIGN_*
	align_y: enum; --ALIGN_*
	segments: tr.Segs;
	shaped: bool;
	wrapped: bool;
	caret_width: num;
	caret_color: color;
	caret_insert_mode: bool;
	selectable: bool;
	selection: tr.Selection;
}

--NOTE: segments not initialized here.
local default_text = constant(`LayerText {
	runs = tr.TextRuns(nil);
	align_x = ALIGN_CENTER;
	align_y = ALIGN_CENTER;
	caret_width = 1;
	caret_color = color {0xffffffff};
	selectable = true;
})

struct LayoutSolver {
	type       : enum; --LAYOUT_*
	axis_order : enum; --AXIS_ORDER_*
	init       : {&Layer} -> {};
	free       : {&Layer} -> {};
	sync       : {&Layer} -> {};
	sync_min_w : {&Layer, bool} -> num;
	sync_min_h : {&Layer, bool} -> num;
	sync_x     : {&Layer, bool} -> bool;
	sync_y     : {&Layer, bool} -> bool;
	sync_top   : {&Layer, num, num} -> {};
}

FLEX_FLOW_X = 1
FLEX_FLOW_Y = 2

struct FlexboxLayout {
	--common to flexbox & grid
	align_items_x: enum;  --ALIGN_*
	align_items_y: enum;  --ALIGN_*
 	item_align_x: enum;   --ALIGN_*
	item_align_y: enum;   --ALIGN_*

	flow: enum; --FLEX_FLOW_*
	wrap: bool;

	__reserved: uint8[2]; --to appease the freelist
}

local default_flexbox_layout = constant(`FlexboxLayout {
	align_items_x = ALIGN_STRETCH;
	align_items_y = ALIGN_STRETCH;
	item_align_x  = ALIGN_STRETCH;
	item_align_y  = ALIGN_STRETCH;

	flow = FLEX_FLOW_X;
	wrap = false;
})

terra FlexboxLayout:init()
	@self = default_flexbox_layout
end

terra FlexboxLayout:free() end

struct GridLayoutCol {
	x: num;
	w: num;
	fr: num;
	align_x: enum;
	_min_w: num;
	snap_x: bool;
	inlayout: bool;
}

struct GridLayout {
	--common to flexbox & grid
	align_items_x: enum;  --ALIGN_*
	align_items_y: enum;  --ALIGN_*
 	item_align_x: enum;   --ALIGN_*
	item_align_y: enum;   --ALIGN_*

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

terra GridLayout:init()
	self.align_items_x = ALIGN_STRETCH
	self.align_items_y = ALIGN_STRETCH
	self.item_align_x  = ALIGN_STRETCH
	self.item_align_y  = ALIGN_STRETCH

	self.cols:init()
	self.rows:init()
	self._cols:init()
	self._rows:init()
end

terra GridLayout:free()
	self.cols:free()
	self.rows:free()
	self._cols:free()
	self._rows:free()
end

struct LayerManager {
	tr: tr.TextRenderer;
	grid_occupied: BoolBitmap;

	layers          : freelist(Layer);
	borders         : freelist(LayerBorder);
	backgrounds     : freelist(LayerBackground);
	shadows         : freelist(LayerShadow);
	texts           : freelist(LayerText);
	fonts           : freelist(LayerFont);
	flexbox_layouts : freelist(FlexboxLayout);
	grid_layouts    : freelist(GridLayout);
}

struct Layer (gettersandsetters) {

	manager: &LayerManager;

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

	border: &LayerBorder;
	background: &LayerBackground;
	shadow: &LayerShadow;

	padding_left   : num;
	padding_right  : num;
	padding_top    : num;
	padding_bottom : num;

	visible: bool;
	opacity: num;
	clip_content: enum; --CLIP_CONTENT_*

	text: &LayerText;

	--layouts -----------------------------------------------------------------

	layout: &LayoutSolver; --current layout implementation

	--all layouts
	_min_w: num;
	_min_h: num;

	--flex and grid layouts
	min_cw: num;
	min_ch: num;
	union {
		flex: &FlexboxLayout;
		grid: &GridLayout;
	}

	--flex or grid parent layout
	align_x: enum; --ALIGN_*
	align_y: enum; --ALIGN_*

	--child of flex layout
	fr: num;
	break_before: bool;
	break_after : bool;

	--child of grid layout
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

--border geometry and drawing ------------------------------------------------

--border edge widths relative to box rect at %-offset in border width.
--offset is in -1..1 where -1=inner edge, 0=center, 1=outer edge.
--returned widths are positive when inside and negative when outside box rect.
terra LayerBorder:edge_widths(offset: num, max_w: num, max_h: num)
	var o = self.offset + offset + 1
	var w1 = lerp(o, -1, 1, self.left,   0)
	var h1 = lerp(o, -1, 1, self.top,    0)
	var w2 = lerp(o, -1, 1, self.right,  0)
	var h2 = lerp(o, -1, 1, self.bottom, 0)
	--adjust overlapping widths by scaling them down proportionally.
	if w1 + w2 > max_w or h1 + h2 > max_h then
		var scale = min(max_w / (w1 + w2), max_h / (h1 + h2))
		w1 = w1 * scale
		h1 = h1 * scale
		w2 = w2 * scale
		h2 = h2 * scale
	end
	return w1, h1, w2, h2
end

--border rect at %-offset in border width.
terra Layer:border_rect(offset: num, size_offset: num)
	var w1, h1, w2, h2 = self.border:edge_widths(offset, self.w, self.h)
	var w = self.w - w2 - w1
	var h = self.h - h2 - h1
	return box2d_offset(size_offset, w1, h1, w, h)
end

--corner radius at pixel offset from the stroke's center on one dimension.
local terra offset_radius(r: num, o: num)
	return iif(r > 0, max(0.0, r + o), 0.0)
end

--border rect at %-offset in border width, plus radii of rounded corners.
terra Layer:border_round_rect(offset: num, size_offset: num)

	var k = self.border.corner_radius_kappa

	var x1, y1, w, h = self:border_rect(0, 0) --at stroke center
	var X1, Y1, W, H = self:border_rect(offset, size_offset) --at offset

	var x2, y2 = x1 + w, y1 + h
	var X2, Y2 = X1 + W, Y1 + H

	var r1 = self.border.corner_radius_top_left
	var r2 = self.border.corner_radius_top_right
	var r3 = self.border.corner_radius_bottom_right
	var r4 = self.border.corner_radius_bottom_left

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

terra Layer:border_line_to(cr: &cairo_t, x: num, y: num, q: num)
	if self.border.line_to ~= nil then
		self.border.line_to(self, cr, x, y, q)
	end
end

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

terra Layer:border_visible()
	return
		   self.border.left   ~= 0
		or self.border.top    ~= 0
		or self.border.right  ~= 0
		or self.border.bottom ~= 0
end

terra Layer:draw_border(cr: &cairo_t)
	if not self:border_visible() then return end

	--seamless drawing when all side colors are the same.
	if self.border.color_left.uint == self.border.color_top.uint
		and self.border.color_left.uint == self.border.color_right.uint
		and self.border.color_left.uint == self.border.color_bottom.uint
	then
		cr:new_path()
		cr:rgba(self.border.color_bottom)
		if self.border.left == self.border.top
			and self.border.left == self.border.right
			and self.border.left == self.border.bottom
		then --stroke-based terra (doesn't require path offseting; supports dashing)
			self:border_path(cr, 0, 0)
			cr:line_width(self.border.left)
			if self.border.dash.len > 0 then
				cr:dash(self.border.dash.elements, self.border.dash.len, self.border.dash_offset)
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

	if self.border.color_left.alpha > 0 then
		cr:new_path()
		cr:move_to(x1, y1+r1y)
		self:corner_path(cr, x1+r1x, y1+r1y, r1x, r1y, 1, .5, k)
		self:corner_path(cr, X1+R1X, Y1+R1Y, R1X, R1Y, 1.5, -.5, K)
		cr:line_to(X1, Y2-R4Y)
		self:corner_path(cr, X1+R4X, Y2-R4Y, R4X, R4Y, 5, -.5, K)
		self:corner_path(cr, x1+r4x, y2-r4y, r4x, r4y, 4.5, .5, k)
		cr:close_path()
		cr:rgba(self.border.color_left)
		cr:fill()
	end

	if self.border.color_top.alpha > 0 then
		cr:new_path()
		cr:move_to(x2-r2x, y1)
		self:corner_path(cr, x2-r2x, y1+r2y, r2x, r2y, 2, .5, k)
		self:corner_path(cr, X2-R2X, Y1+R2Y, R2X, R2Y, 2.5, -.5, K)
		cr:line_to(X1+R1X, Y1)
		self:corner_path(cr, X1+R1X, Y1+R1Y, R1X, R1Y, 2, -.5, K)
		self:corner_path(cr, x1+r1x, y1+r1y, r1x, r1y, 1.5, .5, k)
		cr:close_path()
		cr:rgba(self.border.color_top)
		cr:fill()
	end

	if self.border.color_right.alpha > 0 then
		cr:new_path()
		cr:move_to(x2, y2-r3y)
		self:corner_path(cr, x2-r3x, y2-r3y, r3x, r3y, 3, .5, k)
		self:corner_path(cr, X2-R3X, Y2-R3Y, R3X, R3Y, 3.5, -.5, K)
		cr:line_to(X2, Y1+R2Y)
		self:corner_path(cr, X2-R2X, Y1+R2Y, R2X, R2Y, 3, -.5, K)
		self:corner_path(cr, x2-r2x, y1+r2y, r2x, r2y, 2.5, .5, k)
		cr:close_path()
		cr:rgba(self.border.color_right)
		cr:fill()
	end

	if self.border.color_bottom.alpha > 0 then
		cr:new_path()
		cr:move_to(x1+r4x, y2)
		self:corner_path(cr, x1+r4x, y2-r4y, r4x, r4y, 4, .5, k)
		self:corner_path(cr, X1+R4X, Y2-R4Y, R4X, R4Y, 4.5, -.5, K)
		cr:line_to(X2-R3X, Y2)
		self:corner_path(cr, X2-R3X, Y2-R3Y, R3X, R3Y, 4, -.5, K)
		self:corner_path(cr, x2-r3x, y2-r3y, r3x, r3y, 3.5, .5, k)
		cr:close_path()
		cr:rgba(self.border.color_bottom)
		cr:fill()
	end
end

--background geometry and drawing --------------------------------------------

terra LayerBackground:get_type()
	return self._type
end

terra LayerBackground:set_type(type: enum)
	if self._type == type then return end
	free(self._pattern)
	if self._type == BACKGROUND_TYPE_GRADIENT then
		self._gradient:free()
	elseif self._type == BACKGROUND_TYPE_IMAGE then
		self._image:free()
	end
	if type == BACKGROUND_TYPE_COLOR then
		self._color = color {0}
	elseif type == BACKGROUND_TYPE_GRADIENT then
		self._gradient:init()
	elseif type == BACKGROUND_TYPE_IMAGE then
		self._image:init()
	end
	self._type = type
end

terra Layer:background_visible()
	return self.background.type ~= BACKGROUND_TYPE_NONE
end

terra LayerBackground:init()
	@self = default_background
end

terra LayerBackground:free()
	self.type = BACKGROUND_TYPE_NONE
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

terra LayerBackground:get_color()
	assert(self.type == BACKGROUND_TYPE_COLOR)
	return &self._color
end

terra LayerBackground:get_gradient()
	assert(self.type == BACKGROUND_TYPE_GRADIENT)
	return &self._gradient
end

terra LayerBackground:get_image()
	assert(self.type == BACKGROUND_TYPE_IMAGE)
	return &self._image
end

terra LayerBackground:get_newcolor()
	self.type = BACKGROUND_TYPE_COLOR
	return &self._color
end

terra LayerBackground:get_newgradient()
	self.type = BACKGROUND_TYPE_GRADIENT
	return &self._gradient
end

terra LayerBackground:get_newimage()
	self.type = BACKGROUND_TYPE_IMAGE
	return &self._image
end

terra LayerBackground:get_pattern()
	if self._pattern == nil then
		if self.type == BACKGROUND_TYPE_GRADIENT then
			var g = self._gradient
			if g.type == GRADIENT_TYPE_LINEAR then
				var p = g._points
				self._pattern = cairo_pattern_create_linear(p.x1, p.y1, p.x2, p.y2)
			else
				var c = g._circles
				self._pattern = cairo_pattern_create_radial(c.cx1, c.cy1, c.r1, c.cx2, c.cy2, c.r2)
			end
			for _,c in g.color_stops do
				self._pattern:add_color_stop_rgba(c.offset, c.color)
			end
		elseif self.type == BACKGROUND_TYPE_IMAGE then
			self._pattern = cairo_pattern_create_for_surface(self._image:surface())
		end
	end
	return self._pattern
end

terra LayerBackground:paint(cr: &cairo_t)
	cr:operator(self.operator)
	if self.type == BACKGROUND_TYPE_COLOR then
		cr:rgba(self._color)
		cr:paint()
		return
	end
	var m: matrix; m:init()
	m:translate(
		self.x,
		self.y)
	if self.rotation ~= 0 then
		m:rotate_around(
			self.rotation_cx,
			self.rotation_cy,
			rad(self.rotation))
	end
	if self.scale ~= 1 then
		m:scale_around(
			self.scale_cx,
			self.scale_cy,
			self.scale,
			self.scale)
	end
	m:invert()
	var patt = self.pattern
	patt:matrix(&m)
	patt:extend(self.extend)
	cr:source(patt)
	cr:paint()
	cr:rgb(0, 0, 0) --release source
end

--box-shadow geometry and drawing --------------------------------------------

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
		free(self.shadow._state.blurred_surface)
		self.shadow._state.blurred_surface = bmp:surface()
	end
	var sx = bx + self.shadow.x
	var sy = by + self.shadow.y
	cr:translate(sx, sy)
	cr:rgba(self.shadow.color)
	cr:mask(self.shadow._state.blurred_surface, 0, 0)
	cr:translate(-sx, -sy)
end

terra LayerShadow:init(layer: &Layer)
	self._state.blur:init(
		boxblur.BITMAP_FORMAT_G8,
		[boxblur.RepaintFunc]([Layer:getmethod'repaint_shadow']),
		self)
end

terra LayerShadow:free()
	free(self._state.blurred_surface)
	self._state.blur:free()
end

terra LayerShadow:invalidate()
	free(self._state.blurred_surface)
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

--text geometry & drawing ----------------------------------------------------

terra LayerText:init(layer: &Layer)
	@self = default_text
	self.segments:init(&layer.manager.tr)
end

terra LayerText:free()
	self.segments:free()
	self.runs:free()
end

terra Layer:text_visible()
	return self.text.runs.array.len > 0
		and self.text.runs.array:at(0).font ~= nil
		and self.text.runs.array:at(0).font_size > 0
		and self.text.runs.text.len > 0
end

terra Layer:sync_text_shape()
	if not self:text_visible() then return false end
	if self.text.shaped then return true end
	self.manager.tr:shape(&self.text.runs, &self.text.segments)
	self.text.shaped = true
	return true
end

terra Layer:sync_text_wrap()
	if self.text.wrapped then return end
	self.text.segments:wrap(self.cw)
	self.text.wrapped = true
end

terra Layer:sync_text_align()
	self.text.segments:align(0, 0, self.cw, self.ch,
		self.text.align_x, self.text.align_y)
	if self.text.selectable then
		self.text.selection:init(&self.text.segments)
	end
end

terra Layer:get_baseline()
	if not self:text_visible() then return self.h end
	return self.text.segments.lines.baseline
end

terra Layer:draw_text(cr: &cairo_t)
	if not self:text_visible() then return end
	var x1: double, y1: double, x2: double, y2: double
	cr:clip_extents(&x1, &y1, &x2, &y2)
	self.text.segments:clip(x1, y1, x2-x1, y2-y1)
	self.manager.tr:paint(cr, &self.text.segments)
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

--drawing & hit testing ------------------------------------------------------

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
	var bg = self.background.type ~= BACKGROUND_TYPE_NONE

	self:draw_shadow(cr)

	var clip = bg or cc
	if clip then
		cr:save()
		cr:new_path()
		self:background_path(cr, 0) --'background' clipping is implicit here
		cr:clip()
		if bg then
			self.background:paint(cr)
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
terra Layer:sync_layout()          self.layout.sync(self) end
terra Layer:sync_min_w(b: bool)    return self.layout.sync_min_w(self, b) end
terra Layer:sync_min_h(b: bool)    return self.layout.sync_min_h(self, b) end
terra Layer:sync_layout_x(b: bool) return self.layout.sync_x(self, b) end
terra Layer:sync_layout_y(b: bool) return self.layout.sync_y(self, b) end

terra Layer:sync(w: num, h: num)
	self.layout.sync_top(self, w, h)
	self:sync_layout()
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
	axis_order = iif(axis_order ~= 0, axis_order, self.layout.axis_order)
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
local terra null_sync(self: &Layer)
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
local terra null_sync_min_w(self: &Layer, other_axis_synced: bool)
	self._min_w = snap_up(self.min_cw + self.pw, self.snap_x)
	return self._min_w
end

--called by flexible layouts to know the minimum height of their children.
--width-in-height-out layouts call this only after w and x are sync'ed.
local terra null_sync_min_h(self: &Layer, other_axis_synced: bool)
	self._min_h = snap_up(self.min_ch + self.ph, self.snap_y)
	return self._min_h
end

--called by flexible layouts to sync their children on one axis. in response,
--null-layouts sync themselves and their children on both axes when the
--second axis is synced.
local terra null_sync_x(self: &Layer, other_axis_synced: bool)
	if other_axis_synced then
		self:sync_layout()
	end
	return true
end

local terra null_sync_top(self: &Layer, w: num, h: num)
	self.x = 0
	self.y = 0
	self.w = w
	self.h = h
end

local null_layout = constant(`LayoutSolver {
	type       = LAYOUT_NULL;
	axis_order = 0;
	init       = nil;
	free       = nil;
	sync       = null_sync;
	sync_min_w = null_sync_min_w;
	sync_min_h = null_sync_min_h;
	sync_x     = null_sync_x;
	sync_y     = null_sync_x;
	sync_top   = null_sync_top;
})

local terra sync_top(self: &Layer, w: num, h: num) --for all other layout types
	self.min_cw = w - self.pw
	self.min_ch = h - self.ph
end

--textbox layout -------------------------------------------------------------

local terra textbox_sync(self: &Layer)
	if not self.visible then return end
	if self:sync_text_shape() then
		self.cw = 0
		self.ch = 0
		return
	end
	self.cw = max(self.text.segments:min_w(), self.min_cw)
	self:sync_text_wrap()
	self.cw = max(self.text.segments.lines.max_ax, self.min_cw)
	self.ch = max(self.min_ch, self.text.segments.lines.spaced_h)
	self.x, self.w = self:snapxw(self.x, self.w)
	self.y, self.h = self:snapyh(self.y, self.h)
	self:sync_text_align()
	self:sync_layout_children()
end

local terra textbox_sync_min_w(self: &Layer, other_axis_synced: bool)
	var min_cw: num
	if not other_axis_synced then --TODO: or self.nowrap
		min_cw = iif(self:sync_text_shape(), self.text.segments:min_w(), 0)
	else
		--height-in-width-out parent layout with wrapping text not supported
		min_cw = 0
	end
	min_cw = max(min_cw, self.min_cw)
	var min_w = snap_up(min_cw + self.pw, self.snap_x)
	self._min_w = min_w
	return min_w
end

local terra textbox_sync_min_h(self: &Layer, other_axis_synced: bool)
	var min_ch: num
	if other_axis_synced then --TODO: or self.nowrap
		min_ch = self.text.segments.lines.spaced_h
	else
		--height-in-width-out parent layout with wrapping text not supported
		min_ch = 0
	end
	min_ch = max(min_ch, self.min_ch)
	var min_h = snap_up(min_ch + self.ph, self.snap_y)
	self._min_h = min_h
	return min_h
end

local terra textbox_sync_x(self: &Layer, other_axis_synced: bool)
	if not other_axis_synced then
		self:sync_text_wrap()
		return true
	end
end

local terra textbox_sync_y(self: &Layer, other_axis_synced: bool)
	if other_axis_synced then
		self:sync_text_align()
		self:sync_layout_children()
		return true
	end
end

local textbox_layout = constant(`LayoutSolver {
	type       = LAYOUT_TEXTBOX;
	axis_order = 0;
	init       = nil;
	free       = nil;
	sync       = textbox_sync;
	sync_min_w = textbox_sync_min_w;
	sync_min_h = textbox_sync_min_h;
	sync_x     = textbox_sync_x;
	sync_y     = textbox_sync_x;
	sync_top   = sync_top;
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
				total_fr = total_fr + max(0.0, item.fr)
			end
		end
		total_fr = max(1.0, total_fr) --treat sub-unit fractions like css flexbox

		--compute the total overflow width and total free width.
		var total_overflow_w = 0
		var total_free_w = 0
		for i = i, j do
			var item = items:at(i)
			if item.inlayout then
				var min_w = item.[_MIN_W]
				var flex_w = total_w * max(0.0, item.fr) / total_fr
				var overflow_w = max(0.0, min_w - flex_w)
				var free_w = max(0.0, flex_w - min_w)
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
			self.children, i, j, self.[CW], self.flex.[ITEM_ALIGN_X], moving)
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
	Layer.methods['flexbox_sync_x_'..X] = terra(
		self: &Layer, other_axis_synced: bool, align_baseline: bool
	)
		var align = self.flex.[ALIGN_ITEMS_X]
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
		var align = self.flex.[ITEM_ALIGN_Y]
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
		var align = self.flex.[ALIGN_ITEMS_Y]
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

local terra flexbox_sync_min_w(self: &Layer, other_axis_synced: bool)

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

local terra flexbox_sync_min_h(self: &Layer, other_axis_synced: bool)

	var align_baseline = self.flex.flow == FLEX_FLOW_X
		and self.flex.item_align_y == ALIGN_BASELINE

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

local terra flexbox_sync_x(self: &Layer, other_axis_synced: bool)

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

local terra flexbox_sync_y(self: &Layer, other_axis_synced: bool)

	if self.flex.flow == FLEX_FLOW_X and self.flex.item_align_y == ALIGN_BASELINE then
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

local terra flexbox_sync(self: &Layer)
	self:sync_layout_separate_axes(0, -inf, -inf)
end

local terra flexbox_init(self: &Layer)
	self.flex:init()
end

local terra flexbox_free(self: &Layer)
	self.flex:free()
end

local flexbox_layout = constant(`LayoutSolver {
	type       = LAYOUT_FLEXBOX;
	axis_order = AXIS_ORDER_XY;
	init       = flexbox_init;
	free       = flexbox_free;
	sync       = flexbox_sync;
	sync_min_w = flexbox_sync_min_w;
	sync_min_h = flexbox_sync_min_h;
	sync_x     = flexbox_sync_x;
	sync_y     = flexbox_sync_x;
	sync_top   = sync_top;
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
		self.bits.len = rows * cols
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

terra Layer:sync_layout_grid_autopos()

	var flow = self.grid.flow
	var col_first = (flow and GRID_FLOW_Y) == 0
	var row_first = not col_first
	var flip_cols = (flow and GRID_FLOW_R) ~= 0
	var flip_rows = (flow and GRID_FLOW_B) ~= 0
	var grid_wrap = max(1, self.grid.wrap)
	var max_col = iif(col_first, grid_wrap, self.grid.min_lines)
	var max_row = iif(row_first, grid_wrap, self.grid.min_lines)

	var occupied = self.manager.grid_occupied
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
				layer:['sync_min_'..W](other_axis_synced) --recurse
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
		cols.len = max_col

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
		var align_items_x = self.grid.[ALIGN_ITEMS_X]
		var item_align_x = self.grid.[ITEM_ALIGN_X]
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
				layer:['sync_layout_'..X](other_axis_synced) --recurse
			end
		end
		return true
	end

	return sync_min_w, sync_x
end
local grid_sync_min_w, grid_sync_x = gen_funcs('x', 'y', 'w', 'h', 'col', ALIGN_LEFT, ALIGN_RIGHT)
local grid_sync_min_h, grid_sync_y = gen_funcs('y', 'x', 'h', 'w', 'row', ALIGN_TOP, ALIGN_BOTTOM)

local terra grid_sync(self: &Layer)
	self:sync_layout_grid_autopos()
	self:sync_layout_separate_axes(0, -inf, -inf)
end

local terra grid_init(self: &Layer)
	self.grid:init()
end

local terra grid_free(self: &Layer)
	self.grid:free()
end

local grid_layout = constant(`LayoutSolver {
	type       = LAYOUT_GRID;
	axis_order = AXIS_ORDER_XY;
	init       = grid_init;
	free       = grid_free;
	sync       = grid_sync;
	sync_min_w = grid_sync_min_w;
	sync_min_h = grid_sync_min_h;
	sync_x     = grid_sync_x;
	sync_y     = grid_sync_x;
	sync_top   = sync_top;
})

--layout plugin vtable -------------------------------------------------------

--NOTE: layouts must be added in the order of LAYOUT_* constants.
local layouts = constant(`arrayof(LayoutSolver,
	null_layout,
	textbox_layout,
	flexbox_layout,
	grid_layout
))

terra Layer:get_layout_type() return self.layout.type end

terra Layer:set_layout_type(type: enum)
	if self.layout.free ~= nil then
		self.layout.free(self)
	end
	self.layout = &layouts[type]
	if self.layout.init ~= nil then
		self.layout.init(self)
	end
end

--init/free ------------------------------------------------------------------

terra Layer:init(manager: &LayerManager)

	fill(self)

	self.manager = manager
	self.children:init()

	self.scale = 1.0
	self.snap_x = true
	self.snap_y = true

	self.background = &default_background
	self.border     = &default_border
	self.shadow     = &default_shadow
	self.text       = &default_text

	self.visible = true
	self.opacity = 1

	self.layout = &null_layout
end

terra Layer:free(): {}
	self.children:free()

	if self.border ~= &default_border then
		self.border:free()
		self.manager.borders:release(self.border)
		self.border = &default_border
	end

	if self.background ~= &default_background then
		self.background:free()
		self.manager.backgrounds:release(self.background)
		self.background = &default_background
	end

	if self.shadow ~= &default_shadow then
		self.shadow:free()
		self.manager.shadows:release(self.shadow)
		self.shadow = &default_shadow
	end

	if self.text ~= &default_text then
		self.text:free()
		self.manager.texts:release(self.text)
		self.text = &default_text
	end

	self.layout_type = LAYOUT_NULL
	if self.parent ~= nil then
		self.parent.children:remove(self)
	else
		self.manager.layers:release(self)
	end
end

--layer manager --------------------------------------------------------------

terra LayerManager:init()
	self.layers          :init()
	self.borders         :init()
	self.backgrounds     :init()
	self.shadows         :init()
	self.tr              :init()
	self.texts           :init()
	self.fonts           :init()
	self.flexbox_layouts :init()
	self.grid_layouts    :init()
	self.grid_occupied   :init()
end

terra LayerManager:free()
	self.layers:free()
	self.borders         :free()
	self.backgrounds     :free()
	self.shadows         :free()
	self.tr              :free()
	self.texts           :free()
	self.fonts           :free()
	self.flexbox_layouts :free()
	self.grid_layouts    :free()
	self.grid_occupied   :free()
end

terra LayerManager:layer()
	var e = self.layers:alloc()
	assert(e ~= nil)
	e:init(self)
	return e
end

terra layer_manager()
	var man = alloc(LayerManager); man:init()
	return man
end

--C/ffi module ---------------------------------------------------------------

--child layer management

terra Layer:get_parent() return self.parent end
terra Layer:get_index(): int return self.parent.children:index(self) end

terra Layer:layer(i: int) return self.children:at(i) end
terra Layer:get_layer_count() return self.children.len end

terra Layer:layer_insert(i: int)
	var e = self.children:add()
	e:init(self.manager)
	e.parent = self
	return e
end

terra Layer:layer_remove(i: int)
	var e = self.children:at(i); assert(e ~= nil)
	e:free()
end

terra Layer:layer_move(i1: int, i2: int) --negative indices allowed
	self.children:move(i1, i2)
end

terra Layer:move(parent: &Layer, i: int)
	var e1 = iif(parent ~= nil,
		parent:layer_insert(i),
		self.manager:layer())
	@e1 = @self
	e1.parent = parent
	if self.parent ~= nil then
		self.parent.children:remove(self)
	else
		self.manager.layers:release(self)
	end
	return e1
end

--position and size

terra Layer:get_x() return self.x end
terra Layer:get_y() return self.y end
terra Layer:get_w() return self.w end
terra Layer:get_h() return self.h end

terra Layer:set_x(v: num) self.x = v end
terra Layer:set_y(v: num) self.y = v end
terra Layer:set_w(v: num) self.w = v; self.shadow:invalidate() end
terra Layer:set_h(v: num) self.h = v; self.shadow:invalidate() end

terra Layer:get_min_cw() return self.min_cw end
terra Layer:get_min_ch() return self.min_cw end

terra Layer:set_min_cw(v: num) self.min_cw = v end
terra Layer:set_min_ch(v: num) self.min_ch = v end

do end --borders (and fix for terra issue #358)

terra Layer:get_newborder()
	if self.border == &default_border then
		self.border = self.manager.borders:alloc()
		self.border:init()
	end
	return self.border
end

terra Layer:get_border_left   () return self.border.left   end
terra Layer:get_border_right  () return self.border.right  end
terra Layer:get_border_top    () return self.border.top    end
terra Layer:get_border_bottom () return self.border.bottom end

terra Layer:set_border_left   (v: num) self.newborder.left   = v; self.shadow:invalidate() end
terra Layer:set_border_right  (v: num) self.newborder.right  = v; self.shadow:invalidate() end
terra Layer:set_border_top    (v: num) self.newborder.top    = v; self.shadow:invalidate() end
terra Layer:set_border_bottom (v: num) self.newborder.bottom = v; self.shadow:invalidate() end

terra Layer:get_corner_radius_top_left     () return self.border.corner_radius_top_left     end
terra Layer:get_corner_radius_top_right    () return self.border.corner_radius_top_right    end
terra Layer:get_corner_radius_bottom_left  () return self.border.corner_radius_bottom_left  end
terra Layer:get_corner_radius_bottom_right () return self.border.corner_radius_bottom_right end
terra Layer:get_corner_radius_kappa        () return self.border.corner_radius_kappa        end

terra Layer:set_corner_radius_top_left     (v: num) self.newborder.corner_radius_top_left     = v; self.shadow:invalidate() end
terra Layer:set_corner_radius_top_right    (v: num) self.newborder.corner_radius_top_right    = v; self.shadow:invalidate() end
terra Layer:set_corner_radius_bottom_left  (v: num) self.newborder.corner_radius_bottom_left  = v; self.shadow:invalidate() end
terra Layer:set_corner_radius_bottom_right (v: num) self.newborder.corner_radius_bottom_right = v; self.shadow:invalidate() end
terra Layer:set_corner_radius_kappa        (v: num) self.newborder.corner_radius_kappa        = v; self.shadow:invalidate() end

terra Layer:get_border_color_left   () return self.border.color_left   .uint end
terra Layer:get_border_color_right  () return self.border.color_right  .uint end
terra Layer:get_border_color_top    () return self.border.color_top    .uint end
terra Layer:get_border_color_bottom () return self.border.color_bottom .uint end

terra Layer:set_border_color_left   (v: uint32) self.newborder.color_left   .uint = v end
terra Layer:set_border_color_right  (v: uint32) self.newborder.color_right  .uint = v end
terra Layer:set_border_color_top    (v: uint32) self.newborder.color_top    .uint = v end
terra Layer:set_border_color_bottom (v: uint32) self.newborder.color_bottom .uint = v end

terra Layer:get_border_dash_count() return self.border.dash.len end
terra Layer:clear_border_dash() self.border.dash.len = 0 end
terra Layer:get_border_dash(i: int) return self.newborder.dash(i) end
terra Layer:set_border_dash(i: int, v: num) return self.newborder.dash:set(i, v) end
terra Layer:get_border_dash_offset() return self.border.dash_offset end
terra Layer:set_border_dash_offset(v: int) self.newborder.dash_offset = v end

terra Layer:get_border_offset() return self.border.offset end
terra Layer:set_border_offset(v: int) self.newborder.offset = v; self.shadow:invalidate() end

terra Layer:set_border_line_to(line_to: BorderLineToFunc)
	self.newborder.line_to = line_to; self.shadow:invalidate()
end

do end --backgrounds

terra Layer:get_newbackground()
	if self.background == &default_background then
		self.background = self.manager.backgrounds:alloc()
		self.background:init()
	end
	free(self.background._pattern)
	return self.background
end

terra Layer:get_background_type() return self.background.type end
terra Layer:set_background_type(v: enum) self.newbackground.type = v end

terra Layer:get_background_color() return self.background.color.uint end
terra Layer:set_background_color(v: uint) self.newbackground.newcolor.uint = v end

terra Layer:get_background_gradient_type() return self.background.gradient.type end
terra Layer:set_background_gradient_type(v: enum) self.newbackground.newgradient.type = v end

terra Layer:get_background_gradient_x1() return self.background.gradient.points.x1 end
terra Layer:get_background_gradient_y1() return self.background.gradient.points.y1 end
terra Layer:get_background_gradient_x2() return self.background.gradient.points.x2 end
terra Layer:get_background_gradient_y2() return self.background.gradient.points.y2 end

terra Layer:set_background_gradient_x1(x1: num) self.newbackground.newgradient.newpoints.x1 = x1 end
terra Layer:set_background_gradient_y1(y1: num) self.newbackground.newgradient.newpoints.y1 = y1 end
terra Layer:set_background_gradient_x2(x2: num) self.newbackground.newgradient.newpoints.x2 = x2 end
terra Layer:set_background_gradient_y2(y2: num) self.newbackground.newgradient.newpoints.y2 = y2 end

terra Layer:get_background_gradient_cx1() return self.background.gradient.circles.cx1 end
terra Layer:get_background_gradient_cy1() return self.background.gradient.circles.cy1 end
terra Layer:get_background_gradient_cx2() return self.background.gradient.circles.cx2 end
terra Layer:get_background_gradient_cy2() return self.background.gradient.circles.cy2 end
terra Layer:get_background_gradient_r1 () return self.background.gradient.circles.r1  end
terra Layer:get_background_gradient_r2 () return self.background.gradient.circles.r2  end

terra Layer:set_background_gradient_cx1(cx1: num) self.newbackground.newgradient.newcircles.cx1 = cx1 end
terra Layer:set_background_gradient_cy1(cy1: num) self.newbackground.newgradient.newcircles.cy1 = cy1 end
terra Layer:set_background_gradient_cx2(cx2: num) self.newbackground.newgradient.newcircles.cx2 = cx2 end
terra Layer:set_background_gradient_cy2(cy2: num) self.newbackground.newgradient.newcircles.cy2 = cy2 end
terra Layer:set_background_gradient_r1 (r1 : num) self.newbackground.newgradient.newcircles.r1  = r1  end
terra Layer:set_background_gradient_r2 (r2 : num) self.newbackground.newgradient.newcircles.r2  = r2  end

terra Layer:get_background_gradient_color_stops_count()
	return self.background.gradient.color_stops.len
end
terra Layer:clear_background_gradient_color_stops()
	self.background.gradient.color_stops.len = 0
end
terra Layer:get_background_gradient_color_stops_color(i: int)
	var cs = self.background.gradient.color_stops:at(i); assert(cs ~= nil)
	return cs.color.uint
end
terra Layer:get_background_gradient_color_stops_offset(i: int)
	var cs = self.background.gradient.color_stops:at(i); assert(cs ~= nil)
	return cs.offset
end
terra Layer:set_background_gradient_color_stops_color(i: int, color: uint32)
	self.newbackground.newgradient.color_stops:set(i).color.uint = color
end
terra Layer:set_background_gradient_color_stops_offset(i: int, offset: num)
	self.newbackground.newgradient.color_stops:set(i).offset = offset
end

terra Layer:get_background_image() return self.background.image end
terra Layer:set_background_image(v: &Bitmap) @self.newbackground.newimage = @v end

terra Layer:get_background_hittable    () return self.background.hittable end
terra Layer:get_background_clip_border_offset() return self.background.clip_border_offset end
terra Layer:get_background_operator    () return self.background.operator end
terra Layer:get_background_x           () return self.background.x end
terra Layer:get_background_y           () return self.background.y end
terra Layer:get_background_rotation    () return self.background.rotation end
terra Layer:get_background_rotation_cx () return self.background.rotation_cx end
terra Layer:get_background_rotation_cy () return self.background.rotation_cy end
terra Layer:get_background_scale       () return self.background.scale end
terra Layer:get_background_scale_cx    () return self.background.scale_cx end
terra Layer:get_background_scale_cy    () return self.background.scale_cy end
terra Layer:get_background_extend      () return self.background.extend end

terra Layer:set_background_hittable    (v: bool) self.newbackground.hittable = v end
terra Layer:set_background_clip_border_offset(v: num) self.newbackground.clip_border_offset = v; self.shadow:invalidate() end
terra Layer:set_background_operator    (v: enum) self.newbackground.operator = v end
terra Layer:set_background_x           (v: num)  self.newbackground.x = v end
terra Layer:set_background_y           (v: num)  self.newbackground.y = v end
terra Layer:set_background_rotation    (v: num)  self.newbackground.rotation = v end
terra Layer:set_background_rotation_cx (v: num)  self.newbackground.rotation_cx = v end
terra Layer:set_background_rotation_cy (v: num)  self.newbackground.rotation_cy = v end
terra Layer:set_background_scale       (v: num)  self.newbackground.scale = v end
terra Layer:set_background_scale_cx    (v: num)  self.newbackground.scale_cx = v end
terra Layer:set_background_scale_cy    (v: num)  self.newbackground.scale_cy = v end
terra Layer:set_background_extend      (v: enum) self.newbackground.extend = v end

do end --shadows

terra Layer:get_newshadow()
	if self.shadow == &default_shadow then
		self.shadow = self.manager.shadows:alloc()
		self.shadow:init(self)
	end
	return self.shadow
end

terra Layer:get_shadow_x      () return self.shadow.x end
terra Layer:get_shadow_y      () return self.shadow.x end
terra Layer:get_shadow_color  () return self.shadow.color end
terra Layer:get_shadow_blur   () return self.shadow.blur end
terra Layer:get_shadow_passes () return self.shadow.passes end

terra Layer:set_shadow_x      (v: num)    self.newshadow.x          = v end
terra Layer:set_shadow_y      (v: num)    self.newshadow.x          = v end
terra Layer:set_shadow_color  (v: uint32) self.newshadow.color.uint = v end
terra Layer:set_shadow_blur   (v: uint8)  self.newshadow.blur       = v; self.shadow:invalidate() end
terra Layer:set_shadow_passes (v: uint8)  self.newshadow.passes     = v; self.shadow:invalidate() end

do end --text

terra Layer:get_newtext()
	if self.text == &default_text then
		self.text = self.manager.texts:alloc()
		self.text:init(self)
	end
	return self.text
end

terra Layer:get_text_utf32() return self.text.runs.text.elements end
terra Layer:get_text_utf32_len() return self.text.runs.text.len end

terra Layer:set_text_utf32(s: &codepoint, len: int)
	var t = &self.newtext.runs.text
	t.len = 0
	t:add(s, len)
	self.text.shaped = false
end

struct TextRun (gettersandsetters) {
	t: tr.TextRun;
}
TextRun.metamethods.__typename_ffi = 'TextRun'

terra TextRun:unshape() self.t._state.t.shaped = false end
terra TextRun:unwrap() self.t._state.t.wrapped = false end

terra TextRun:get_feature_count() self.t.features.len end
terra TextRun:feature_clear() self.t.features.len = 0; self:unshape() end

terra TextRun:feature_get(i: int, buf: &char, len: int)
	var f = self.t.features:at(i); assert(f ~= nil)
	hb_feature_to_string(f, buf, len)
end

terra TextRun:feature_set(i: int, s: rawstring, len: int)
	var f = self.t.features:set(i); assert(f ~= nil)
	if hb_feature_from_string(s, len, f) ~= 0 then
		self:unshape()
		return true
	else
		return false
	end
end

terra TextRun:get_offset            () return self.t.offset end
terra TextRun:get_font              () return [&LayerFont](self.t.font) end
terra TextRun:get_font_size         () return self.t.font_size end
terra TextRun:get_script            () return self.t.script end
terra TextRun:get_lang              () return self.t.lang end
terra TextRun:get_dir               () return self.t.dir end
terra TextRun:get_line_spacing      () return self.t.line_spacing end
terra TextRun:get_hardline_spacing  () return self.t.hardline_spacing end
terra TextRun:get_paragraph_spacing () return self.t.paragraph_spacing end
terra TextRun:get_nowrap            () return self.t.nowrap end
terra TextRun:get_color             () return self.t.color.uint end
terra TextRun:get_opacity           () return self.t.opacity end
terra TextRun:get_operator          () return self.t.operator end

terra TextRun:set_offset            (v: int)            self.t.offset = v    ; self:unshape() end
terra TextRun:set_font              (v: &LayerFont)     self.t.font = &v.f;  ; self:unshape() end
terra TextRun:set_font_size         (v: num)            self.t.font_size = v ; self:unshape() end
terra TextRun:set_script            (v: hb_script_t)    self.t.script = v    ; self:unshape() end
terra TextRun:set_lang              (v: hb_language_t)  self.t.lang = v      ; self:unshape() end
terra TextRun:set_dir               (v: FriBidiParType) self.t.dir = v       ; self:unshape() end
terra TextRun:set_line_spacing      (v: num)            self.t.line_spacing = v      ; self:unwrap() end
terra TextRun:set_hardline_spacing  (v: num)            self.t.hardline_spacing = v  ; self:unwrap() end
terra TextRun:set_paragraph_spacing (v: num)            self.t.paragraph_spacing = v ; self:unwrap() end
terra TextRun:set_nowrap            (v: bool)           self.t.nowrap = v            ; self:unwrap() end
terra TextRun:set_color             (v: uint32)         self.t.color.uint = v end
terra TextRun:set_opacity           (v: double)         self.t.opacity = v    end
terra TextRun:set_operator          (v: int)            self.t.operator = v   end

terra Layer:get_text_run_count() return self.newtext.runs.array.len end
terra Layer:text_run_clear()
	self.newtext.runs.array.len = 0
	self.text.shaped = false
end
terra Layer:text_run(i: int)
	var a = &self.newtext.runs.array
	var t = a:at(i, nil)
	if t == nil then
		t = a:set(i)
		t:init()
		t._state.t = self.text
		self.text.shaped = false
	end
	return [&TextRun](t)
end

terra Layer:get_align_x() return self.text.align_x end
terra Layer:get_align_y() return self.text.align_y end

terra Layer:set_align_x(v: enum) self.newtext.align_x = v end
terra Layer:set_align_y(v: enum) self.newtext.align_y = v end

terra Layer:get_caret_width()       return self.text.caret_width end
terra Layer:get_caret_color()       return self.text.caret_color.uint end
terra Layer:get_caret_insert_mode() return self.text.caret_insert_mode end
terra Layer:get_selectable()        return self.text.selectable end

terra Layer:set_caret_width(v: num)        self.newtext.caret_width = v end
terra Layer:set_caret_color(v: uint32)     self.newtext.caret_color.uint = v end
terra Layer:set_caret_insert_mode(v: bool) self.newtext.caret_insert_mode = v end
terra Layer:set_selectable(v: bool)        self.newtext.selectable = v end

--fonts

terra LayerFont:init(manager: &LayerManager, load: tr.FontLoadFunc, unload: tr.FontUnloadFunc)
	self.f:init(&manager.tr, load, unload)
end

terra LayerFont:free()
	assert(self.f.refcount == 0)
	memfree(self)
end

terra LayerManager:font(load: tr.FontLoadFunc, unload: tr.FontUnloadFunc)
	var font = alloc(LayerFont)
	font:init(self, load, unload)
	return font
end

do end --layouts

--[[
terra Layer:get_newflex()
	if self.flex == &default_flexbox_layout then
		self.flex = self.manager.flexbox_layouts:alloc()
		self.flex:init()
	end
	return self.flex
end

terra Layer:get_flex_align_items_x() return self.flex.align_items_x end
terra Layer:set_flex_align_items_x(v: num) self.newflex.align_items_x = v end
]]

--publish & bulid

function build(self)
	local public = publish'layerlib'

	public(TextRun, {

		get_feature_count=1,
		feature_clear=1,
		feature_get=1,
		feature_set=1,

		get_offset            =1,
		get_font              =1,
		get_font_size         =1,
		get_script            =1,
		get_lang              =1,
		get_dir               =1,
		get_line_spacing      =1,
		get_hardline_spacing  =1,
		get_paragraph_spacing =1,
		get_nowrap            =1,
		get_color             =1,
		get_opacity           =1,
		get_operator          =1,

		set_offset            =1,
		set_font              =1,
		set_font_size         =1,
		set_script            =1,
		set_lang              =1,
		set_dir               =1,
		set_line_spacing      =1,
		set_hardline_spacing  =1,
		set_paragraph_spacing =1,
		set_nowrap            =1,
		set_color             =1,
		set_opacity           =1,
		set_operator          =1,
	}, true)

	public(LayerFont, {
		free=1,
	}, true)

	public(layer_manager)

	public(Layer, {

		free=1,
		draw=1,
		sync=1,

		--position in hierarchy

		get_parent=1,
		get_index=1,
		layer=1,
		get_layer_count=1,
		layer_insert=1,
		layer_remove=1,
		layer_move=1,
		move=1,

		--size and position

		get_x=1,
		get_y=1,
		get_w=1,
		get_h=1,
		set_x=1,
		set_y=1,
		set_w=1,
		set_h=1,

		get_cx=1,
		get_cy=1,
		get_cw=1,
		get_ch=1,
		set_cx=1,
		set_cy=1,
		set_cw=1,
		set_ch=1,

		get_min_cw=1,
		get_min_ch=1,
		set_min_cw=1,
		set_min_ch=1,

		to_parent=1, from_parent=1,
		to_window=1, from_window=1,

		--borders

		get_border_left   =1,
		get_border_right  =1,
		get_border_top    =1,
		get_border_bottom =1,
		set_border_left   =1,
		set_border_right  =1,
		set_border_top    =1,
		set_border_bottom =1,

		get_corner_radius_top_left     =1,
		get_corner_radius_top_right    =1,
		get_corner_radius_bottom_left  =1,
		get_corner_radius_bottom_right =1,
		get_corner_radius_kappa        =1,
		set_corner_radius_top_left     =1,
		set_corner_radius_top_right    =1,
		set_corner_radius_bottom_left  =1,
		set_corner_radius_bottom_right =1,
		set_corner_radius_kappa        =1,

		get_border_color_left   =1,
		get_border_color_right  =1,
		get_border_color_top    =1,
		get_border_color_bottom =1,
		set_border_color_left   =1,
		set_border_color_right  =1,
		set_border_color_top    =1,
		set_border_color_bottom =1,

		get_border_dash_count=1,
		clear_border_dash=1,
		get_border_dash=1,
		set_border_dash=1,
		get_border_dash_offset=1,
		set_border_dash_offset=1,

		set_border_line_to=1,

		--backgrounds

		get_background_type=1,
		set_background_type=1,

		get_background_color=1,
		set_background_color=1,

		get_background_gradient_type=1,
		set_background_gradient_type=1,

		get_background_gradient_x1=1,
		get_background_gradient_y1=1,
		get_background_gradient_x2=1,
		get_background_gradient_y2=1,

		set_background_gradient_x1=1,
		set_background_gradient_y1=1,
		set_background_gradient_x2=1,
		set_background_gradient_y2=1,

		get_background_gradient_cx1=1,
		get_background_gradient_cy1=1,
		get_background_gradient_cx2=1,
		get_background_gradient_cy2=1,
		get_background_gradient_r1 =1,
		get_background_gradient_r2 =1,

		set_background_gradient_cx1=1,
		set_background_gradient_cy1=1,
		set_background_gradient_cx2=1,
		set_background_gradient_cy2=1,
		set_background_gradient_r1 =1,
		set_background_gradient_r2 =1,

		get_background_gradient_color_stops_count=1,
		clear_background_gradient_color_stops=1,
		get_background_gradient_color_stops_color=1,
		set_background_gradient_color_stops_color=1,
		get_background_gradient_color_stops_offset=1,
		set_background_gradient_color_stops_offset=1,

		get_background_image=1,
		set_background_image=1,

		get_background_hittable    =1,
		get_background_clip_border_offset=1,
		get_background_operator    =1,
		get_background_x           =1,
		get_background_y           =1,
		get_background_rotation    =1,
		get_background_rotation_cx =1,
		get_background_rotation_cy =1,
		get_background_scale       =1,
		get_background_scale_cx    =1,
		get_background_scale_cy    =1,
		get_background_extend      =1,

		set_background_hittable    =1,
		set_background_clip_border_offset=1,
		set_background_operator    =1,
		set_background_x           =1,
		set_background_y           =1,
		set_background_rotation    =1,
		set_background_rotation_cx =1,
		set_background_rotation_cy =1,
		set_background_scale       =1,
		set_background_scale_cx    =1,
		set_background_scale_cy    =1,
		set_background_extend      =1,

		--shadows

		get_shadow_x      =1,
		get_shadow_y      =1,
		get_shadow_color  =1,
		get_shadow_blur   =1,
		get_shadow_passes =1,

		set_shadow_x      =1,
		set_shadow_y      =1,
		set_shadow_color  =1,
		set_shadow_blur   =1,
		set_shadow_passes =1,

		--text

		get_text_utf32=1,
		get_text_utf32_len=1,
		set_text_utf32=1,

		get_text_run_count=1,
		text_run_clear=1,
		text_run=1,

		get_align_x=1,
		get_align_y=1,

		set_align_x=1,
		set_align_y=1,

		get_caret_width=1,
		get_caret_color=1,
		get_caret_insert_mode=1,
		get_selectable=1,

		set_caret_width=1,
		set_caret_color=1,
		set_caret_insert_mode=1,
		set_selectable=1,

		--layouts


	}, true)

	public(LayerManager, {
		layer=1,
		font=1,
		free=1,
	}, true)

	public:getenums(layerlib)

	public:build{
		linkto = {'cairo', 'freetype', 'harfbuzz', 'fribidi', 'unibreak', 'boxblur', 'xxhash'},
	}
end

if not ... then
	print'Compiling...'
	build()
	print(sizeof(Layer), 'sizeof(Layer)')
end

return layerlib
