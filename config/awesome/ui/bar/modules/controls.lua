local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
local helpers = require("helpers")

local volume = require("ui.widgets").volume
local clock = require("ui.widgets").clock
local cpu = require("ui.widgets").cpu
local mem = require("ui.widgets").mem
local fs = require("ui.widgets").fs

local widget = wibox.widget({
	widget = wibox.container.background,
	shape = helpers.ui.rrect(beautiful.border_radius),
	bg = beautiful.color0,
	{
		widget = wibox.container.margin,
		margins = dpi(10),
		{
			layout = wibox.layout.fixed.horizontal,
			spacing = dpi(20),
			fs(),
			mem(),
			cpu(),
			volume(),
			clock(),
		},
	},
})

return widget
