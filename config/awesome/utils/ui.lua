local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
local M = { placement = {} }
local capi = { mouse = mouse }

-- Client helpers
M.placement.centered = function(c)
	return gears.timer.delayed_call(function()
		awful.placement.centered(c, {
			honor_padding = true,
			honor_workarea = true,
		})
	end)
end

-- UI Helpers
M.spacer = function()
	return wibox.widget({ widget = wibox.container.margin, left = 5, right = 5 })
end

M.rrect = function(radius)
	radius = radius or dpi(4)
	return function(cr, width, height)
		gears.shape.rounded_rect(cr, width, height, radius)
	end
end

M.prect = function(tl, tr, br, bl, radius)
	radius = radius or dpi(4)
	return function(cr, width, height)
		gears.shape.partially_rounded_rect(cr, width, height, tl, tr, br, bl, radius)
	end
end

M.colorize_text = function(txt, fg)
	if fg == "" then
		fg = "#ffffff"
	end

	return "<span foreground='" .. fg .. "'>" .. txt .. "</span>"
end

M.vertical_pad = function(height)
	return wibox.widget({
		forced_height = height,
		layout = wibox.layout.fixed.vertical,
	})
end

M.horizontal_pad = function(width)
	return wibox.widget({
		forced_width = width,
		layout = wibox.layout.fixed.horizontal,
	})
end

M.add_hover_cursor = function(w, hover_cursor)
	local original_cursor = "left_ptr"

	w:connect_signal("mouse::enter", function()
		local widget = capi.mouse.current_wibox
		if widget then
			widget.cursor = hover_cursor
		end
	end)

	w:connect_signal("mouse::leave", function()
		local widget = capi.mouse.current_wibox
		if widget then
			widget.cursor = original_cursor
		end
	end)
end

return M
