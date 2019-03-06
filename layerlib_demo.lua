--go @ luajit -jv *

local ffi = require'ffi'
local nw = require'nw'
local ll = require'layerlib_h'

local win = nw:app():window{
	w = 800, h = 500,
}

local lm = ll.layer_manager()
local e = lm:layer()

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
	e.border_color_right = 0xffffffff
	e.corner_radius_top_left = 20

	e.background_gradient_y2 = 100
	e:background_gradient_color_stops_offset_set(0, 0)
	e:background_gradient_color_stops_offset_set(1, 1)
	e:background_gradient_color_stops_color_set(0, 0xff0000ff)
	e:background_gradient_color_stops_color_set(1, 0x0000ffff)
	--e.background_gradient_cx1 = 1
	--e.background_color = 0x0000ffff --0x336699ff

	e:draw(cr)

	self:invalidate()
end

nw:app():run()
e:free()
lm:free()
