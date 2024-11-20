local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
local naughty = require("naughty")
local gears = require("gears")
local helpers = require("helpers")

local M = {}

--- Notification icon container
M.icon = function(ico_image)
	local notification_icon = wibox.widget({
		{
			id = "icon",
			resize = true,
			forced_height = dpi(25),
			forced_width = dpi(25),
			widget = wibox.widget.imagebox,
		},
		layout = wibox.layout.fixed.horizontal,
	})
	notification_icon.icon:set_image(ico_image)
	return notification_icon
end

--- Notification title container
M.title = function(title)
	return wibox.widget({
		markup = title,
		font = beautiful.notification_font .. "Bold 12",
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox,
	})
end

--- Notification message container
M.message = function(msg)
	return wibox.widget({
		markup = msg,
		font = beautiful.notification_font .. "Regular 11",
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox,
	})
end

--- Notification app name container
M.appname = function(app)
	return wibox.widget({
		markup = app,
		font = beautiful.notification_font .. "Bold 12",
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox,
	})
end

--- Notification actions container
M.actions = function(n)
	local actions_template = wibox.widget({
		notification = n,
		base_layout = wibox.widget({
			spacing = dpi(0),
			layout = wibox.layout.flex.horizontal,
		}),
		widget_template = {
			{
				{
					{
						id = "text_role",
						font = beautiful.notification_font .. "Regular 10",
						widget = wibox.widget.textbox,
					},
					widget = wibox.container.place,
				},
				bg = beautiful.notification_panel_bg_alt,
				shape = gears.shape.rounded_rect,
				forced_height = dpi(30),
				widget = wibox.container.background,
			},
			margins = dpi(4),
			widget = wibox.container.margin,
		},
		style = { underline_normal = false, underline_selected = true },
		widget = naughty.list.actions,
	})

	helpers.ui.add_hover_cursor(actions_template, "hand1")
	return actions_template
end

--- Notification dismiss button
M.dismiss = function()
	local dismiss_textbox = wibox.widget({
		{
			id = "dismiss_icon",
			font = beautiful.icon_font .. "11",
			markup = helpers.ui.colorize_text("󰩹", beautiful.fg),
			align = "center",
			valign = "center",
			widget = wibox.widget.textbox,
		},
		layout = wibox.layout.fixed.horizontal,
	})

	local dismiss = wibox.widget({
		{
			dismiss_textbox,
			margins = dpi(5),
			widget = wibox.container.margin,
		},
		visible = false,
		bg = beautiful.notification_panel_bg_alt,
		shape = gears.shape.circle,
		widget = wibox.container.background,
	})

	helpers.ui.add_hover_cursor(dismiss, "hand2")
	return dismiss
end

return M
