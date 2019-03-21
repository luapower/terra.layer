--go @ luajit -joff -jv *
jit.off(true, true)

local ffi = require'ffi'
local nw = require'nw'
local ll = require'layerlib_h'
local glue = require'glue'
local cairo = require'cairo'

local win = nw:app():window{
	w = 800, h = 500,
}

local function load_font(self, file_data_buf, file_size_buf)
	local s = glue.readfile'media/fonts/OpenSans-Regular.ttf'
	local p = glue.malloc(#s)
	file_data_buf[0] = p
	ffi.copy(file_data_buf[0], s, #s)
	file_size_buf[0] = #s
end

local function unload_font(self, file_data_buf, file_size_buf)
	glue.free(file_data_buf[0])
	file_data_buf[0] = nil
	file_size_buf[0] = 0
end

local s = glue.readfile('winapi_history.md')

local function test()
	local man = ll.layer_manager()
	local e = man:layer()
	local font = man:font(load_font, unload_font)

	for i=1,1 do

		local sr = cairo.image_surface('bgra8', 500, 500)
		local cr = sr:context()

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

		e:set_text_utf8(s, -1)
		local t = e:text_run(0)
		t.font = font
		t.font_size = 16
		t.color = 0xffffffff

		e:sync(500, 500)
		e:draw(cr)

		e:clear_text_runs()

		sr:save_png'layerlib_demo.png'

		cr:free()
		sr:free()
	end

	man:dump_stats()

	e:free()
	man:free()
end
test()
