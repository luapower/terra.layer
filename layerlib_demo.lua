--go @ luajit -joff -jv *
jit.off(true, true)

local ffi = require'ffi'
local nw = require'nw'
local ll = require'layerlib_h'
local glue = require'glue'
local cairo = require'cairo'

local app = nw:app()

local win = app:window{
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

local man = ll.layer_manager()
local font = man:font(load_font, unload_font)

local e = man:layer()

function win:repaint()

	local cr = self:bitmap():cairo()

	cr:rgba(0, 0, 0, 1)
	cr:paint()

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
	--e:set_text_span_font_id(0, 0)
	e:set_text_span_font_size(0, 16)
	e:set_text_span_color(0, 0xffffffff)

	e:sync(self:client_size())
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
