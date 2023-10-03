local awful = require("awful")
local wibox = require("wibox")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

-- Titlebars
client.connect_signal("request::titlebars", function(c)
	titlebar = awful.titlebar(c, { size = dpi(20) })
	titlebar.widget = {
		{ -- Left
			{
				awful.titlebar.widget.iconwidget(c),
				layout = wibox.layout.fixed.horizontal,
			},
			widget = wibox.container.margin,
			margins = 5,
		},
		{ -- Middle
			{ -- Title
				halign = "center",
				widget = awful.titlebar.widget.titlewidget(c),
			},
			layout = wibox.layout.flex.horizontal,
		},
		{ -- Right
			layout = wibox.layout.fixed.horizontal(),
		},
		layout = wibox.layout.align.horizontal,
	}
end)
