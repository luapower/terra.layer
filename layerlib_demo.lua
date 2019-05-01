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

local function load_font(self, file_data_buf, file_size_buf)
	local s = glue.readfile'media/fonts/OpenSans-Regular.ttf'
	local p = glue.malloc(#s)
	file_data_buf[0] = p
	ffi.copy(file_data_buf[0], s, #s)
	file_size_buf[0] = #s
	print('load_font', file_data_buf[0], file_size_buf[0])
end

local function unload_font(self, file_data_buf, file_size_buf)
	print('unload_font', file_data_buf[0])
	glue.free(file_data_buf[0])
	file_data_buf[0] = nil
	file_size_buf[0] = 0
end

local s = glue.readfile('lorem_ipsum.txt')--:sub(1, 20)

assert(ll.memtotal() == 0)

local man = ll.layer_manager()
local font_id = man:font(load_font, unload_font)
local e = man:layer()

--man.subpixel_x_resolution = 1/2
--man.glyph_cache_size = 0
man.glyph_run_cache_size = 0

function win:repaint()

	local cr = self:bitmap():cairo()

	cr:rgba(1, 1, 1, 1)
	cr:paint()

	--e.border_left = 2
	--e.border_top = 2
	--e.border_right = 10
	--e.border_color_left  = 0xffffffff
	--e.border_color_top   = 0xffff00ff
	--e.border_color_right = 0x008800ff
	--e.corner_radius_top_left = 20

	--e.background_type = ll.BACKGROUND_TYPE_LINEAR_GRADIENT
	--e.background_gradient_y2 = 100
	--e:set_background_gradient_color_stops_offset(0, 0)
	--e:set_background_gradient_color_stops_offset(1, 1)
	--e:set_background_gradient_color_stops_color(0, 0xff0000ff)
	--e:set_background_gradient_color_stops_color(1, 0x0000ffff)
	--e.background_gradient_cx1 = 1

	e.background_type = ll.BACKGROUND_TYPE_COLOR
	e.background_color = 0x000000ff --0x336699ff

	if not self.xx then
		e:set_text_utf8(s, -1)
		e:set_text_span_script   (0, 'Zyyy')
		e:set_text_span_lang     (0, 'en')
		e:set_text_span_font_id  (0, font_id)
		e:set_text_span_font_size(0, 14)
		e:set_text_span_color    (0, 0xffffffff)
		self.xx = true
	end

	e.text_align_y = ll.ALIGN_TOP
	e.text_align_x = ll.ALIGN_CENTER

	local w, h = self:client_size()
	e:sync(w - 20, h - 20)
	e:draw(cr)

	--e:clear_text_runs()
end

function win:keyup(key)
	if key == 'esc' then
		self:close()
	end
end

app:run()
man:dump_stats()
man:free()

ll.memreport()
