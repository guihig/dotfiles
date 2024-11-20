local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

-- Titlebars
client.connect_signal("request::titlebars", function(c)
	local state_icon = wibox.widget({
		text = c.floating and beautiful.icons.float or beautiful.icons.tile,
		align = "center",
		valign = "center",
		font = beautiful.icon .. "10",
		widget = wibox.widget.textbox,
	})

	state_icon.update = function()
		state_icon:set_text(c.floating and beautiful.icons.float or beautiful.icons.tile)
	end

	c:connect_signal("property::floating", state_icon.update)

	local titlebar = awful.titlebar(c, { size = dpi(20) })
	titlebar.widget = {
		{ -- Left
			{
				layout = wibox.layout.fixed.horizontal,
				awful.titlebar.widget.iconwidget(c),
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
			{
				layout = wibox.layout.fixed.horizontal,
				state_icon,
			},
			widget = wibox.container.margin,
			margins = 5,
		},
		layout = wibox.layout.align.horizontal,
	}
end)
