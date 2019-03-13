--go @ luajit -jv *

local ffi = require'ffi'
local nw = require'nw'
local ll = require'layerlib_h'
local glue = require'glue'

local win = nw:app():window{
	w = 800, h = 500,
}

local lm = ll.layer_manager()
local e = lm:layer()

local function load_font(self, file_data_buf, file_size_buf)
	local s = glue.readfile'media/fonts/OpenSans-Regular.ttf'
	file_data_buf[0] = ffi.cast('void*', s)
	file_size_buf[0] = #s
end
local function unload_font(self, file_data_buf, file_size_buf)
	ffi.free(file_data_buf[0])
	file_data_buf[0] = nil
	file_size_buf[0] = 0
end
local font = lm:font(load_font, unload_font)

function win:repaint()
	local cr = win:bitmap():cairo()
	cr:rgba(0, 0, 0, 1)
	cr:paint()

	e.x = 100
	e.y = 100
	e.w = 100
	e.h = 100
	e.border_left = 2
	e.border_top = 2
	e.border_right = 10
	e.border_color_left  = 0xffffffff
	e.border_color_top   = 0xffff00ff
	e.border_color_right = 0x008800ff
	e.corner_radius_top_left = 20

	e.background_gradient_y2 = 100
	e:set_background_gradient_color_stops_offset(0, 0)
	e:set_background_gradient_color_stops_offset(1, 1)
	e:set_background_gradient_color_stops_color(0, 0xff0000ff)
	e:set_background_gradient_color_stops_color(1, 0x0000ffff)
	--e.background_gradient_cx1 = 1
	--e.background_color = 0x0000ffff --0x336699ff

	local s = 'H\0\0\0e\0\0\0l\0\0\0l\0\0\0o\0\0\0'
	local p = ffi.cast('const char*', s)
	e:set_text_utf32(ffi.cast('uint32_t*', p), #s / 4)
	local t = e:text_run(0)
	t.font = font
	t.font_size = 16

	e:sync(self:client_size())
	e:draw(cr)

	--os.exit()
	self:invalidate()
end

nw:app():run()
e:free()
lm:free()
