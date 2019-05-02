local ffi = require'ffi'
local C = ffi.load'layerlib'
ffi.cdef[[
typedef struct Layer Layer;
typedef struct _cairo _cairo;
typedef struct double2 double2;
typedef struct Bitmap Bitmap;
typedef struct cairo_argb32_color_t cairo_argb32_color_t;
typedef struct LayerManager LayerManager;
typedef struct Font Font;
typedef void (*BorderLineToFunc) (Layer*, _cairo*, double, double, double);
typedef void (*FontUnloadFunc) (Font*, void**, int64_t*);
uint64_t memtotal();
void memreport();
void Layer_draw(Layer*, _cairo*);
void Layer_sync(Layer*, double, double);
double Layer_get_cx(Layer*);
double Layer_get_cy(Layer*);
double Layer_get_cw(Layer*);
double Layer_get_ch(Layer*);
void Layer_set_cw(Layer*, double);
void Layer_set_ch(Layer*, double);
double2 Layer_to_parent(Layer*, double, double);
double2 Layer_from_parent(Layer*, double, double);
double2 Layer_to_window(Layer*, double, double);
double2 Layer_from_window(Layer*, double, double);
Layer* Layer_get_parent(Layer*);
int32_t Layer_get_index(Layer*);
Layer* Layer_layer(Layer*, int32_t);
int32_t Layer_get_layer_count(Layer*);
Layer* Layer_layer_insert(Layer*, int32_t);
void Layer_layer_remove(Layer*, int32_t);
void Layer_layer_move(Layer*, int32_t, int32_t);
Layer* Layer_move(Layer*, Layer*, int32_t);
double Layer_get_x(Layer*);
double Layer_get_y(Layer*);
double Layer_get_w(Layer*);
double Layer_get_h(Layer*);
void Layer_set_x(Layer*, double);
void Layer_set_y(Layer*, double);
void Layer_set_w(Layer*, double);
void Layer_set_h(Layer*, double);
double Layer_get_padding_left(Layer*);
double Layer_get_padding_top(Layer*);
double Layer_get_padding_right(Layer*);
double Layer_get_padding_bottom(Layer*);
void Layer_set_padding_left(Layer*, double);
void Layer_set_padding_top(Layer*, double);
void Layer_set_padding_right(Layer*, double);
void Layer_set_padding_bottom(Layer*, double);
double Layer_get_min_cw(Layer*);
double Layer_get_min_ch(Layer*);
void Layer_set_min_cw(Layer*, double);
void Layer_set_min_ch(Layer*, double);
double Layer_get_border_left(Layer*);
double Layer_get_border_right(Layer*);
double Layer_get_border_top(Layer*);
double Layer_get_border_bottom(Layer*);
void Layer_set_border_left(Layer*, double);
void Layer_set_border_right(Layer*, double);
void Layer_set_border_top(Layer*, double);
void Layer_set_border_bottom(Layer*, double);
double Layer_get_corner_radius_top_left(Layer*);
double Layer_get_corner_radius_top_right(Layer*);
double Layer_get_corner_radius_bottom_left(Layer*);
double Layer_get_corner_radius_bottom_right(Layer*);
double Layer_get_corner_radius_kappa(Layer*);
void Layer_set_corner_radius_top_left(Layer*, double);
void Layer_set_corner_radius_top_right(Layer*, double);
void Layer_set_corner_radius_bottom_left(Layer*, double);
void Layer_set_corner_radius_bottom_right(Layer*, double);
void Layer_set_corner_radius_kappa(Layer*, double);
uint32_t Layer_get_border_color_left(Layer*);
uint32_t Layer_get_border_color_right(Layer*);
uint32_t Layer_get_border_color_top(Layer*);
uint32_t Layer_get_border_color_bottom(Layer*);
void Layer_set_border_color_left(Layer*, uint32_t);
void Layer_set_border_color_right(Layer*, uint32_t);
void Layer_set_border_color_top(Layer*, uint32_t);
void Layer_set_border_color_bottom(Layer*, uint32_t);
int32_t Layer_get_border_dash_count(Layer*);
void Layer_clear_border_dashes(Layer*);
double Layer_get_border_dash(Layer*, int32_t);
double* Layer_set_border_dash(Layer*, int32_t, double);
int32_t Layer_get_border_dash_offset(Layer*);
void Layer_set_border_dash_offset(Layer*, int32_t);
void Layer_set_border_line_to(Layer*, BorderLineToFunc);
int8_t Layer_get_background_type(Layer*);
void Layer_set_background_type(Layer*, int8_t);
bool Layer_get_background_hittable(Layer*);
double Layer_get_background_clip_border_offset(Layer*);
int8_t Layer_get_background_operator(Layer*);
void Layer_set_background_hittable(Layer*, bool);
void Layer_set_background_clip_border_offset(Layer*, double);
void Layer_set_background_operator(Layer*, int8_t);
uint32_t Layer_get_background_color(Layer*);
void Layer_set_background_color(Layer*, uint32_t);
double Layer_get_background_gradient_x1(Layer*);
double Layer_get_background_gradient_y1(Layer*);
double Layer_get_background_gradient_x2(Layer*);
double Layer_get_background_gradient_y2(Layer*);
void Layer_set_background_gradient_x1(Layer*, double);
void Layer_set_background_gradient_y1(Layer*, double);
void Layer_set_background_gradient_x2(Layer*, double);
void Layer_set_background_gradient_y2(Layer*, double);
double Layer_get_background_gradient_cx1(Layer*);
double Layer_get_background_gradient_cy1(Layer*);
double Layer_get_background_gradient_cx2(Layer*);
double Layer_get_background_gradient_cy2(Layer*);
double Layer_get_background_gradient_r1(Layer*);
double Layer_get_background_gradient_r2(Layer*);
void Layer_set_background_gradient_cx1(Layer*, double);
void Layer_set_background_gradient_cy1(Layer*, double);
void Layer_set_background_gradient_cx2(Layer*, double);
void Layer_set_background_gradient_cy2(Layer*, double);
void Layer_set_background_gradient_r1(Layer*, double);
void Layer_set_background_gradient_r2(Layer*, double);
int32_t Layer_get_background_gradient_color_stops_count(Layer*);
void Layer_clear_background_gradient_color_stops(Layer*);
uint32_t Layer_get_background_gradient_color_stops_color(Layer*, int32_t);
double Layer_get_background_gradient_color_stops_offset(Layer*, int32_t);
void Layer_set_background_gradient_color_stops_color(Layer*, int32_t, uint32_t);
void Layer_set_background_gradient_color_stops_offset(Layer*, int32_t, double);
Bitmap* Layer_get_background_image(Layer*);
void Layer_set_background_image(Layer*, Bitmap*);
double Layer_get_background_x(Layer*);
double Layer_get_background_y(Layer*);
int8_t Layer_get_background_extend(Layer*);
void Layer_set_background_x(Layer*, double);
void Layer_set_background_y(Layer*, double);
void Layer_set_background_extend(Layer*, int8_t);
double Layer_get_background_rotation(Layer*);
double Layer_get_background_rotation_cx(Layer*);
double Layer_get_background_rotation_cy(Layer*);
double Layer_get_background_scale(Layer*);
double Layer_get_background_scale_cx(Layer*);
double Layer_get_background_scale_cy(Layer*);
void Layer_set_background_rotation(Layer*, double);
void Layer_set_background_rotation_cx(Layer*, double);
void Layer_set_background_rotation_cy(Layer*, double);
void Layer_set_background_scale(Layer*, double);
void Layer_set_background_scale_cx(Layer*, double);
void Layer_set_background_scale_cy(Layer*, double);
double Layer_get_shadow_x(Layer*);
double Layer_get_shadow_y(Layer*);
cairo_argb32_color_t Layer_get_shadow_color(Layer*);
uint8_t Layer_get_shadow_blur(Layer*);
uint8_t Layer_get_shadow_passes(Layer*);
void Layer_set_shadow_x(Layer*, double);
void Layer_set_shadow_y(Layer*, double);
void Layer_set_shadow_color(Layer*, uint32_t);
void Layer_set_shadow_blur(Layer*, uint8_t);
void Layer_set_shadow_passes(Layer*, uint8_t);
uint32_t* Layer_get_text_utf32(Layer*);
int32_t Layer_get_text_utf32_len(Layer*);
void Layer_set_text_utf32(Layer*, uint32_t*, int32_t);
void Layer_set_text_utf8(Layer*, const char *, int32_t);
int32_t Layer_get_text_maxlen(Layer*);
void Layer_set_text_maxlen(Layer*, int32_t);
int32_t Layer_get_text_span_count(Layer*);
void Layer_clear_text_spans(Layer*);
int32_t Layer_get_text_span_feature_count(Layer*, int32_t);
void Layer_clear_text_span_features(Layer*, int32_t);
bool Layer_get_text_span_feature(Layer*, int32_t, int32_t, const char *, int32_t);
bool Layer_set_text_span_feature(Layer*, int32_t, int32_t, const char *, int32_t);
int32_t Layer_get_text_span_offset(Layer*, int32_t);
float Layer_get_text_span_font_size(Layer*, int32_t);
uint32_t Layer_get_text_span_dir(Layer*, int32_t);
float Layer_get_text_span_line_spacing(Layer*, int32_t);
float Layer_get_text_span_hardline_spacing(Layer*, int32_t);
float Layer_get_text_span_paragraph_spacing(Layer*, int32_t);
bool Layer_get_text_span_nowrap(Layer*, int32_t);
uint32_t Layer_get_text_span_color(Layer*, int32_t);
double Layer_get_text_span_opacity(Layer*, int32_t);
int32_t Layer_get_text_span_operator(Layer*, int32_t);
void Layer_set_text_span_offset(Layer*, int32_t, int32_t);
void Layer_set_text_span_font_size(Layer*, int32_t, double);
void Layer_set_text_span_dir(Layer*, int32_t, uint32_t);
void Layer_set_text_span_line_spacing(Layer*, int32_t, double);
void Layer_set_text_span_hardline_spacing(Layer*, int32_t, double);
void Layer_set_text_span_paragraph_spacing(Layer*, int32_t, double);
void Layer_set_text_span_nowrap(Layer*, int32_t, bool);
void Layer_set_text_span_color(Layer*, int32_t, uint32_t);
void Layer_set_text_span_opacity(Layer*, int32_t, double);
void Layer_set_text_span_operator(Layer*, int32_t, int32_t);
const char * Layer_get_text_span_script(Layer*, int32_t);
void Layer_set_text_span_script(Layer*, int32_t, const char *);
const char * Layer_get_text_span_lang(Layer*, int32_t);
void Layer_set_text_span_lang(Layer*, int32_t, const char *);
int8_t Layer_get_text_align_x(Layer*);
int8_t Layer_get_text_align_y(Layer*);
void Layer_set_text_align_x(Layer*, int8_t);
void Layer_set_text_align_y(Layer*, int8_t);
double Layer_get_text_caret_width(Layer*);
uint32_t Layer_get_text_caret_color(Layer*);
bool Layer_get_text_caret_insert_mode(Layer*);
bool Layer_get_text_selectable(Layer*);
void Layer_set_text_caret_width(Layer*, double);
void Layer_set_text_caret_color(Layer*, uint32_t);
void Layer_set_text_caret_insert_mode(Layer*, bool);
void Layer_set_text_selectable(Layer*, bool);
int16_t Layer_get_text_span_font_id(Layer*, int32_t);
void Layer_set_text_span_font_id(Layer*, int32_t, int32_t);
LayerManager* layer_manager();
void LayerManager_free_layer(LayerManager*, Layer*);
Layer* LayerManager_layer(LayerManager*);
double LayerManager_get_font_size_resolution(LayerManager*);
double LayerManager_get_subpixel_x_resolution(LayerManager*);
double LayerManager_get_word_subpixel_x_resolution(LayerManager*);
int32_t LayerManager_get_glyph_cache_size(LayerManager*);
int32_t LayerManager_get_glyph_run_cache_size(LayerManager*);
void LayerManager_set_font_size_resolution(LayerManager*, double);
void LayerManager_set_subpixel_x_resolution(LayerManager*, double);
void LayerManager_set_word_subpixel_x_resolution(LayerManager*, double);
void LayerManager_set_glyph_cache_size(LayerManager*, int32_t);
void LayerManager_set_glyph_run_cache_size(LayerManager*, int32_t);
void LayerManager_free(LayerManager*);
int32_t LayerManager_font(LayerManager*, FontUnloadFunc, FontUnloadFunc);
void LayerManager_dump_stats(LayerManager*);
]]
pcall(ffi.cdef, 'struct double2 { double _0; double _1; };')
local getters = {
	cx = C.Layer_get_cx,
	cy = C.Layer_get_cy,
	cw = C.Layer_get_cw,
	ch = C.Layer_get_ch,
	parent = C.Layer_get_parent,
	index = C.Layer_get_index,
	layer_count = C.Layer_get_layer_count,
	x = C.Layer_get_x,
	y = C.Layer_get_y,
	w = C.Layer_get_w,
	h = C.Layer_get_h,
	padding_left = C.Layer_get_padding_left,
	padding_top = C.Layer_get_padding_top,
	padding_right = C.Layer_get_padding_right,
	padding_bottom = C.Layer_get_padding_bottom,
	min_cw = C.Layer_get_min_cw,
	min_ch = C.Layer_get_min_ch,
	border_left = C.Layer_get_border_left,
	border_right = C.Layer_get_border_right,
	border_top = C.Layer_get_border_top,
	border_bottom = C.Layer_get_border_bottom,
	corner_radius_top_left = C.Layer_get_corner_radius_top_left,
	corner_radius_top_right = C.Layer_get_corner_radius_top_right,
	corner_radius_bottom_left = C.Layer_get_corner_radius_bottom_left,
	corner_radius_bottom_right = C.Layer_get_corner_radius_bottom_right,
	corner_radius_kappa = C.Layer_get_corner_radius_kappa,
	border_color_left = C.Layer_get_border_color_left,
	border_color_right = C.Layer_get_border_color_right,
	border_color_top = C.Layer_get_border_color_top,
	border_color_bottom = C.Layer_get_border_color_bottom,
	border_dash_count = C.Layer_get_border_dash_count,
	border_dash_offset = C.Layer_get_border_dash_offset,
	background_type = C.Layer_get_background_type,
	background_hittable = C.Layer_get_background_hittable,
	background_clip_border_offset = C.Layer_get_background_clip_border_offset,
	background_operator = C.Layer_get_background_operator,
	background_color = C.Layer_get_background_color,
	background_gradient_x1 = C.Layer_get_background_gradient_x1,
	background_gradient_y1 = C.Layer_get_background_gradient_y1,
	background_gradient_x2 = C.Layer_get_background_gradient_x2,
	background_gradient_y2 = C.Layer_get_background_gradient_y2,
	background_gradient_cx1 = C.Layer_get_background_gradient_cx1,
	background_gradient_cy1 = C.Layer_get_background_gradient_cy1,
	background_gradient_cx2 = C.Layer_get_background_gradient_cx2,
	background_gradient_cy2 = C.Layer_get_background_gradient_cy2,
	background_gradient_r1 = C.Layer_get_background_gradient_r1,
	background_gradient_r2 = C.Layer_get_background_gradient_r2,
	background_gradient_color_stops_count = C.Layer_get_background_gradient_color_stops_count,
	background_image = C.Layer_get_background_image,
	background_x = C.Layer_get_background_x,
	background_y = C.Layer_get_background_y,
	background_extend = C.Layer_get_background_extend,
	background_rotation = C.Layer_get_background_rotation,
	background_rotation_cx = C.Layer_get_background_rotation_cx,
	background_rotation_cy = C.Layer_get_background_rotation_cy,
	background_scale = C.Layer_get_background_scale,
	background_scale_cx = C.Layer_get_background_scale_cx,
	background_scale_cy = C.Layer_get_background_scale_cy,
	shadow_x = C.Layer_get_shadow_x,
	shadow_y = C.Layer_get_shadow_y,
	shadow_color = C.Layer_get_shadow_color,
	shadow_blur = C.Layer_get_shadow_blur,
	shadow_passes = C.Layer_get_shadow_passes,
	text_utf32 = C.Layer_get_text_utf32,
	text_utf32_len = C.Layer_get_text_utf32_len,
	text_maxlen = C.Layer_get_text_maxlen,
	text_span_count = C.Layer_get_text_span_count,
	text_align_x = C.Layer_get_text_align_x,
	text_align_y = C.Layer_get_text_align_y,
	text_caret_width = C.Layer_get_text_caret_width,
	text_caret_color = C.Layer_get_text_caret_color,
	text_caret_insert_mode = C.Layer_get_text_caret_insert_mode,
	text_selectable = C.Layer_get_text_selectable,
}
local setters = {
	cw = C.Layer_set_cw,
	ch = C.Layer_set_ch,
	x = C.Layer_set_x,
	y = C.Layer_set_y,
	w = C.Layer_set_w,
	h = C.Layer_set_h,
	padding_left = C.Layer_set_padding_left,
	padding_top = C.Layer_set_padding_top,
	padding_right = C.Layer_set_padding_right,
	padding_bottom = C.Layer_set_padding_bottom,
	min_cw = C.Layer_set_min_cw,
	min_ch = C.Layer_set_min_ch,
	border_left = C.Layer_set_border_left,
	border_right = C.Layer_set_border_right,
	border_top = C.Layer_set_border_top,
	border_bottom = C.Layer_set_border_bottom,
	corner_radius_top_left = C.Layer_set_corner_radius_top_left,
	corner_radius_top_right = C.Layer_set_corner_radius_top_right,
	corner_radius_bottom_left = C.Layer_set_corner_radius_bottom_left,
	corner_radius_bottom_right = C.Layer_set_corner_radius_bottom_right,
	corner_radius_kappa = C.Layer_set_corner_radius_kappa,
	border_color_left = C.Layer_set_border_color_left,
	border_color_right = C.Layer_set_border_color_right,
	border_color_top = C.Layer_set_border_color_top,
	border_color_bottom = C.Layer_set_border_color_bottom,
	border_dash_offset = C.Layer_set_border_dash_offset,
	border_line_to = C.Layer_set_border_line_to,
	background_type = C.Layer_set_background_type,
	background_hittable = C.Layer_set_background_hittable,
	background_clip_border_offset = C.Layer_set_background_clip_border_offset,
	background_operator = C.Layer_set_background_operator,
	background_color = C.Layer_set_background_color,
	background_gradient_x1 = C.Layer_set_background_gradient_x1,
	background_gradient_y1 = C.Layer_set_background_gradient_y1,
	background_gradient_x2 = C.Layer_set_background_gradient_x2,
	background_gradient_y2 = C.Layer_set_background_gradient_y2,
	background_gradient_cx1 = C.Layer_set_background_gradient_cx1,
	background_gradient_cy1 = C.Layer_set_background_gradient_cy1,
	background_gradient_cx2 = C.Layer_set_background_gradient_cx2,
	background_gradient_cy2 = C.Layer_set_background_gradient_cy2,
	background_gradient_r1 = C.Layer_set_background_gradient_r1,
	background_gradient_r2 = C.Layer_set_background_gradient_r2,
	background_image = C.Layer_set_background_image,
	background_x = C.Layer_set_background_x,
	background_y = C.Layer_set_background_y,
	background_extend = C.Layer_set_background_extend,
	background_rotation = C.Layer_set_background_rotation,
	background_rotation_cx = C.Layer_set_background_rotation_cx,
	background_rotation_cy = C.Layer_set_background_rotation_cy,
	background_scale = C.Layer_set_background_scale,
	background_scale_cx = C.Layer_set_background_scale_cx,
	background_scale_cy = C.Layer_set_background_scale_cy,
	shadow_x = C.Layer_set_shadow_x,
	shadow_y = C.Layer_set_shadow_y,
	shadow_color = C.Layer_set_shadow_color,
	shadow_blur = C.Layer_set_shadow_blur,
	shadow_passes = C.Layer_set_shadow_passes,
	text_maxlen = C.Layer_set_text_maxlen,
	text_align_x = C.Layer_set_text_align_x,
	text_align_y = C.Layer_set_text_align_y,
	text_caret_width = C.Layer_set_text_caret_width,
	text_caret_color = C.Layer_set_text_caret_color,
	text_caret_insert_mode = C.Layer_set_text_caret_insert_mode,
	text_selectable = C.Layer_set_text_selectable,
}
local methods = {
	draw = C.Layer_draw,
	sync = C.Layer_sync,
	to_parent = C.Layer_to_parent,
	from_parent = C.Layer_from_parent,
	to_window = C.Layer_to_window,
	from_window = C.Layer_from_window,
	layer = C.Layer_layer,
	layer_insert = C.Layer_layer_insert,
	layer_remove = C.Layer_layer_remove,
	layer_move = C.Layer_layer_move,
	move = C.Layer_move,
	clear_border_dashes = C.Layer_clear_border_dashes,
	get_border_dash = C.Layer_get_border_dash,
	set_border_dash = C.Layer_set_border_dash,
	clear_background_gradient_color_stops = C.Layer_clear_background_gradient_color_stops,
	get_background_gradient_color_stops_color = C.Layer_get_background_gradient_color_stops_color,
	get_background_gradient_color_stops_offset = C.Layer_get_background_gradient_color_stops_offset,
	set_background_gradient_color_stops_color = C.Layer_set_background_gradient_color_stops_color,
	set_background_gradient_color_stops_offset = C.Layer_set_background_gradient_color_stops_offset,
	set_text_utf32 = C.Layer_set_text_utf32,
	set_text_utf8 = C.Layer_set_text_utf8,
	clear_text_spans = C.Layer_clear_text_spans,
	get_text_span_feature_count = C.Layer_get_text_span_feature_count,
	clear_text_span_features = C.Layer_clear_text_span_features,
	get_text_span_feature = C.Layer_get_text_span_feature,
	set_text_span_feature = C.Layer_set_text_span_feature,
	get_text_span_offset = C.Layer_get_text_span_offset,
	get_text_span_font_size = C.Layer_get_text_span_font_size,
	get_text_span_dir = C.Layer_get_text_span_dir,
	get_text_span_line_spacing = C.Layer_get_text_span_line_spacing,
	get_text_span_hardline_spacing = C.Layer_get_text_span_hardline_spacing,
	get_text_span_paragraph_spacing = C.Layer_get_text_span_paragraph_spacing,
	get_text_span_nowrap = C.Layer_get_text_span_nowrap,
	get_text_span_color = C.Layer_get_text_span_color,
	get_text_span_opacity = C.Layer_get_text_span_opacity,
	get_text_span_operator = C.Layer_get_text_span_operator,
	set_text_span_offset = C.Layer_set_text_span_offset,
	set_text_span_font_size = C.Layer_set_text_span_font_size,
	set_text_span_dir = C.Layer_set_text_span_dir,
	set_text_span_line_spacing = C.Layer_set_text_span_line_spacing,
	set_text_span_hardline_spacing = C.Layer_set_text_span_hardline_spacing,
	set_text_span_paragraph_spacing = C.Layer_set_text_span_paragraph_spacing,
	set_text_span_nowrap = C.Layer_set_text_span_nowrap,
	set_text_span_color = C.Layer_set_text_span_color,
	set_text_span_opacity = C.Layer_set_text_span_opacity,
	set_text_span_operator = C.Layer_set_text_span_operator,
	get_text_span_script = C.Layer_get_text_span_script,
	set_text_span_script = C.Layer_set_text_span_script,
	get_text_span_lang = C.Layer_get_text_span_lang,
	set_text_span_lang = C.Layer_set_text_span_lang,
	get_text_span_font_id = C.Layer_get_text_span_font_id,
	set_text_span_font_id = C.Layer_set_text_span_font_id,
}
ffi.metatype('Layer', {
	__index = function(self, k)
		local getter = getters[k]
		if getter then return getter(self) end
		return methods[k]
	end,
	__newindex = function(self, k, v)
		local setter = setters[k]
		if not setter then
			error(('field not found: %s'):format(tostring(k)), 2)
		end
		setter(self, v)
	end,
})
local getters = {
	font_size_resolution = C.LayerManager_get_font_size_resolution,
	subpixel_x_resolution = C.LayerManager_get_subpixel_x_resolution,
	word_subpixel_x_resolution = C.LayerManager_get_word_subpixel_x_resolution,
	glyph_cache_size = C.LayerManager_get_glyph_cache_size,
	glyph_run_cache_size = C.LayerManager_get_glyph_run_cache_size,
}
local setters = {
	font_size_resolution = C.LayerManager_set_font_size_resolution,
	subpixel_x_resolution = C.LayerManager_set_subpixel_x_resolution,
	word_subpixel_x_resolution = C.LayerManager_set_word_subpixel_x_resolution,
	glyph_cache_size = C.LayerManager_set_glyph_cache_size,
	glyph_run_cache_size = C.LayerManager_set_glyph_run_cache_size,
}
local methods = {
	free_layer = C.LayerManager_free_layer,
	layer = C.LayerManager_layer,
	free = C.LayerManager_free,
	font = C.LayerManager_font,
	dump_stats = C.LayerManager_dump_stats,
}
ffi.metatype('LayerManager', {
	__index = function(self, k)
		local getter = getters[k]
		if getter then return getter(self) end
		return methods[k]
	end,
	__newindex = function(self, k, v)
		local setter = setters[k]
		if not setter then
			error(('field not found: %s'):format(tostring(k)), 2)
		end
		setter(self, v)
	end,
})
ffi.cdef[[
enum {
	ALIGN_AUTO = 4,
	ALIGN_BASELINE = 11,
	ALIGN_BOTTOM = 2,
	ALIGN_CENTER = 3,
	ALIGN_DEFAULT = 0,
	ALIGN_END = 7,
	ALIGN_LEFT = 1,
	ALIGN_RIGHT = 2,
	ALIGN_SPACE_AROUND = 9,
	ALIGN_SPACE_BETWEEN = 10,
	ALIGN_SPACE_EVENLY = 8,
	ALIGN_START = 6,
	ALIGN_STRETCH = 5,
	ALIGN_TOP = 1,
	AXIS_ORDER_XY = 1,
	AXIS_ORDER_YX = 2,
	BACKGROUND_EXTEND_NONE = 0,
	BACKGROUND_EXTEND_PAD = 3,
	BACKGROUND_EXTEND_REFLECT = 2,
	BACKGROUND_EXTEND_REPEAT = 1,
	BACKGROUND_TYPE_COLOR = 1,
	BACKGROUND_TYPE_GRADIENT = 3,
	BACKGROUND_TYPE_IMAGE = 4,
	BACKGROUND_TYPE_LINEAR_GRADIENT = 2,
	BACKGROUND_TYPE_NONE = 0,
	BACKGROUND_TYPE_RADIAL_GRADIENT = 3,
	CLIP_CONTENT_NOCLIP = 0,
	CLIP_CONTENT_TO_BACKGROUND = 1,
	CLIP_CONTENT_TO_PADDING = 1,
	FLEX_FLOW_X = 0,
	FLEX_FLOW_Y = 1,
	GRID_FLOW_B = 8,
	GRID_FLOW_L = 0,
	GRID_FLOW_R = 4,
	GRID_FLOW_T = 0,
	GRID_FLOW_X = 0,
	GRID_FLOW_Y = 2,
	LAYOUT_FLEX = 2,
	LAYOUT_GRID = 3,
	LAYOUT_NULL = 0,
	LAYOUT_TEXT = 1,
	OPERATOR_ADD = 12,
	OPERATOR_ATOP = 5,
	OPERATOR_CLEAR = 0,
	OPERATOR_COLOR_BURN = 20,
	OPERATOR_COLOR_DODGE = 19,
	OPERATOR_DARKEN = 17,
	OPERATOR_DEST = 6,
	OPERATOR_DEST_ATOP = 10,
	OPERATOR_DEST_IN = 8,
	OPERATOR_DEST_OUT = 9,
	OPERATOR_DEST_OVER = 7,
	OPERATOR_DIFFERENCE = 23,
	OPERATOR_EXCLUSION = 24,
	OPERATOR_HARD_LIGHT = 21,
	OPERATOR_HSL_COLOR = 27,
	OPERATOR_HSL_HUE = 25,
	OPERATOR_HSL_LUMINOSITY = 28,
	OPERATOR_HSL_SATURATION = 26,
	OPERATOR_IN = 3,
	OPERATOR_LIGHTEN = 18,
	OPERATOR_MULTIPLY = 14,
	OPERATOR_OUT = 4,
	OPERATOR_OVER = 2,
	OPERATOR_OVERLAY = 16,
	OPERATOR_SATURATE = 13,
	OPERATOR_SCREEN = 15,
	OPERATOR_SOFT_LIGHT = 22,
	OPERATOR_SOURCE = 1,
	OPERATOR_XOR = 11,
}]]
return C
