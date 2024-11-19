local beautiful = require("beautiful")
local utils = require("utils")
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
	shape = utils.rrect(4),
	bg = beautiful.color0,
	{
		widget = wibox.container.margin,
		margins = dpi(4),
		systray,
	},
})

return widget
