local beautiful = require("beautiful")
local helpers = require("helpers")
local wibox = require("wibox")
local dpi = beautiful.xresources.apply_dpi

local systray = wibox.widget({
	{
		{
			base_size = 18,
			widget = wibox.widget.systray,
		},
		widget = wibox.container.place,
		valign = "center",
	},
	left = dpi(8),
	right = dpi(8),
	widget = wibox.container.margin,
})

local widget = wibox.widget({
	widget = wibox.container.background,
	shape = helpers.ui.rrect(beautiful.border_radius),
	bg = beautiful.color0,
	{
		widget = wibox.container.margin,
		margins = dpi(4),
		systray,
	},
})

return widget
