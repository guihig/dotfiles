local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local naughty = require("naughty")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi

local M = { placement = {} }

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

-- Layout
M.handle_master_count = function()
	local t = awful.screen.focused().selected_tag
	if t.layout.name == "mstab" then
		t.master_count = 0
	else
		t.master_count = 1
	end
end

-- Log
M.log = function(title, message)
	naughty.notification({
		urgency = "info",
		title = tostring(title),
		message = tostring(message),
	})
end

return M
