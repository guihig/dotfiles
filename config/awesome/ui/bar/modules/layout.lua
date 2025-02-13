local wibox = require("wibox")
local awful = require("awful")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
local helpers = require("helpers")

local layouts = awful.widget.layoutbox({
	buttons = {
		awful.button({
			modifiers = {},
			button = 1,
			on_press = function()
				awful.layout.inc(1)
			end,
		}),
		awful.button({
			modifiers = {},
			button = 3,
			on_press = function()
				awful.layout.inc(-1)
			end,
		}),
		awful.button({
			modifiers = {},
			button = 4,
			on_press = function()
				awful.layout.inc(-1)
			end,
		}),
		awful.button({
			modifiers = {},
			button = 5,
			on_press = function()
				awful.layout.inc(1)
			end,
		}),
	},
})

local widget = {
	{
		{
			layouts,
			clip_shape = helpers.ui.rrect(3),
			widget = wibox.container.margin,
		},
		margins = dpi(10),
		widget = wibox.container.margin,
	},
	bg = beautiful.color0,
	shape = helpers.ui.rrect(beautiful.border_radius),
	widget = wibox.container.background,
}

return widget
