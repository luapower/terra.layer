
setfenv(1, require'layerlib')

local terra load_font(self: &LayerFont, file_data: &&opaque, file_size: &int64)
	@file_data, @file_size = readfile'media/fonts/OpenSans-Regular.ttf'
end

local terra unload_font(self: &LayerFont, file_data: &&opaque, file_size: &int64)
	free(@file_data)
	@file_size = 0
end

local s = glue.readfile('winapi_history.md')

terra test()
	var man = layer_manager()
	var e = man:layer()
	var font = man:font(load_font, unload_font)

	for i=0,100 do

		var sr = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 1000, 1000)
		var cr = sr:context()

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

		e:set_text_utf8(s, [#s])
		var t = e:text_run(0)
		t.font = font
		t.font_size = 14
		t.color = 0xffffffff

		e:sync(1000, 1000)
		e:draw(cr)

		e:clear_text_runs()

		sr:save_png'layerlib_test.png'

		cr:free()
		sr:free()
	end

	man:dump_stats()

	e:free()
	man:free()
end
test()
