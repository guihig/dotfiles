local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi

local layout = require("ui.bar.modules.layout")
local tags = require("ui.bar.modules.tags")
local tasks = require("ui.bar.modules.tasks")
local systray = require("ui.bar.modules.systray")
local controls = require("ui.bar.modules.controls")

local function init(s)
	local wibar = awful.wibar({
		position = "top",
		height = dpi(40),
		ontop = false,
		screen = s,
		widget = {
			widget = wibox.container.margin,
			top = dpi(4),
			bottom = dpi(4),
			{
				layout = wibox.layout.align.horizontal,
				{
					widget = wibox.container.margin,
					left = dpi(10),
					{
						widget = wibox.container.place,
						{
							layout = wibox.layout.fixed.horizontal,
							spacing = dpi(10),
							layout,
							tags(s),
						},
					},
				},
				{

					widget = wibox.container.margin,
					right = dpi(10),
					left = dpi(10),
					{
						widget = wibox.container.place,
						{
							layout = wibox.layout.fixed.horizontal,
							spacing = dpi(10),
							tasks(s),
						},
					},
				},
				{
					widget = wibox.container.margin,
					right = dpi(10),
					{
						widget = wibox.container.place,
						{
							layout = wibox.layout.fixed.horizontal,
							spacing = dpi(10),
							controls,
							s == screen.primary and systray or nil,
						},
					},
				},
			},
		},
	})
	return wibar
end

screen.connect_signal("request::desktop_decoration", function(s)
	s.wibox = init(s)
end)
