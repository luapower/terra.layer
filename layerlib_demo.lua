--go @ luajit -joff -jv *
jit.off(true, true)

local ffi = require'ffi'
local nw = require'nw'
local ll = require'layerlib_h'
local glue = require'glue'
local cairo = require'cairo'

local app = nw:app()

local win = app:window{
	w = 1200, h = 700,
}

local s = glue.readfile'media/fonts/OpenSans-Regular.ttf'

local function load_font(self, file_data_buf, file_size_buf)
	file_data_buf[0] = ffi.cast('void*', s)
	file_size_buf[0] = #s
end

local function unload_font(self, file_data_buf, file_size_buf)
	file_data_buf[0] = nil
	file_size_buf[0] = 0
end

local s = glue.readfile('lorem_ipsum.txt')

--assert(ll.memtotal() == 0)

local llib = ll.layerlib()
local font_id = llib:font(load_font, unload_font)
local e = llib:layer(nil)

e.clip = ll.CLIP_PADDING
--e.clip = ll.CLIP_NONE

--e.padding_left   =  2
--e.padding_top    =  2
--e.padding_right  =  2
--e.padding_bottom =  2

--e.border_left   =  2
--e.border_top    =  2
--e.border_right  =  2
--e.border_bottom =  2
e.border_color_left   = 0xff00ffff
e.border_color_top    = 0xffff00ff
e.border_color_right  = 0x008800ff
e.border_color_bottom = 0x888888ff
--e.corner_radius_top_left = 20

e.padding = 20
e.border = 10
e.border_color = 0xff0000ff

e.bg_type = ll.BG_COLOR
e.bg_color = 0x0099ffff --0x336699ff

--e.bg_type = ll.BG_LINEAR_GRADIENT
e.bg_y2 = 100
e:set_bg_color_stop_offset(0, 0)
e:set_bg_color_stop_offset(1, 1)
e:set_bg_color_stop_color(0, 0xff0000ff)
e:set_bg_color_stop_color(1, 0x0000ffff)
e.bg_cx1 = 1

e.shadow_y = 10
e.shadow_x = 10
e.shadow_blur = 10
e.shadow_passes = 3
e.shadow_color = 0x000000ff

e.layout_type = ll.LAYOUT_FLEX
--e.flex_flow = ll.FLEX_FLOW_Y

local e1, e2 = e:child(0), e:child(1)

e1.border = 10; e1.padding = 20
e2.border = 10; e2.padding = 20
e1.border_color = 0xffff00ff
e2.border_color = 0x00ff00ff
e1.min_cw = 10; e1.min_ch = 10
e2.min_cw = 10; e2.min_ch = 10

--llib.subpixel_x_resolution = 1/2
--llib.glyph_cache_size = 0
--llib.glyph_run_cache_size = 0

e1:set_text_utf8(s, -1)
e1:set_text_span_script   (0, 'Zyyy')
e1:set_text_span_lang     (0, 'en')
e1:set_text_span_font_id  (0, font_id)
e1:set_text_span_font_size(0, 14)
e1:set_text_span_color    (0, 0xffffffff)
e1.text_align_y = ll.ALIGN_TOP
e1.text_align_x = ll.ALIGN_CENTER

--e1.visible = false
--e2.visible = false

--e.shadow_inset = true

e.content_shadow_y = 1
e.content_shadow_x = 1
e.content_shadow_blur = 1
e.content_shadow_passes = 3
e.content_shadow_color = 0x000000ff

function win:repaint()

	local cr = self:bitmap():cairo()

	cr:identity_matrix()
	cr:rgba(1, 1, 1, 1)
	cr:paint()

	local w, h = self:client_size()
	cr:translate(50, 50)
	e:sync(w - 100, h - 100)
	e:draw(cr)

	--e1:set_text_utf8('', -1)
	--e:clear_text_runs()
end

function win:keyup(key)
	if key == 'esc' then
		self:close()
	end
end

app:run()

llib:dump_stats()
llib:free()
ll.memreport()
assert(ll.memtotal() == 0)
