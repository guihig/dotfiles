local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")

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

-- Layout
M.handle_master_count = function()
	local t = awful.screen.focused().selected_tag
	if t.layout.name == "mstab" then
		t.master_count = 0
	else
		t.master_count = 1
	end
end

return M
