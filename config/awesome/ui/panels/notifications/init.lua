local awful = require("awful")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local naughty = require("naughty")
local dpi = xresources.apply_dpi
local wibox = require("wibox")
local helpers = require("helpers")

local function clear_button()
	local icon = wibox.widget({
		{
			id = "icon",
			font = beautiful.icon_font .. "16",
			markup = helpers.ui.colorize_text("󰩹", beautiful.fg),
			halign = "center",
			valign = "center",
			widget = wibox.widget.textbox,
		},
		halign = "center",
		valign = "center",
		fill_horizontal = true,
		widget = wibox.container.place,
	})

	local clear_btn = wibox.widget({
		{
			icon,
			margins = dpi(12),
			widget = wibox.container.margin,
		},
		visible = false,
		bg = beautiful.notification_panel_bg,
		shape = helpers.ui.rrect(beautiful.border_radius),
		widget = wibox.container.background,
	})

	clear_btn:connect_signal("mouse::enter", function()
		clear_btn.bg = beautiful.notification_panel_bg_alt
	end)

	clear_btn:connect_signal("mouse::leave", function()
		clear_btn.bg = beautiful.notification_panel_bg
	end)

	clear_btn:buttons(awful.util.table.join(awful.button({}, 1, function()
		awesome.emit_signal("notification_panel::clear")
	end)))

	awesome.connect_signal("notification_panel::clear", function()
		clear_btn.visible = false
	end)

	awesome.connect_signal("notification_panel::added", function()
		clear_btn.visible = true
	end)

	helpers.ui.add_hover_cursor(clear_btn, "hand2")

	return clear_btn
end

return function(s)
	local notification_box = require("ui.panels.notifications.core")(s)

	s.notification_panel = awful.popup({
		type = "dock",
		screen = s,
		minimum_height = s.geometry.height - beautiful.wibar_height,
		maximum_height = s.geometry.height - beautiful.wibar_height,
		minimum_width = dpi(350),
		maximum_width = dpi(350),
		bg = beautiful.transparent,
		ontop = true,
		visible = false,
		placement = function(w)
			awful.placement.top_right(w)
			awful.placement.maximize_vertically(w, { honor_workarea = true, margins = { top = beautiful.useless_gap } })
		end,
		widget = {
			{
				{
					{
						clear_button(),
						margins = dpi(20),
						widget = wibox.container.margin,
					},
					helpers.ui.vertical_pad(dpi(20)),
					{
						notification_box,
						margins = dpi(20),
						widget = wibox.container.margin,
					},
					layout = wibox.layout.fixed.vertical,
				},
				layout = wibox.layout.flex.vertical,
			},
			shape = helpers.ui.prect(beautiful.border_radius * 2, true, false, false, false),
			bg = beautiful.wibar_bg,
			widget = wibox.container.background,
		},
	})

	--- Toggle container visibility
	awesome.connect_signal("notification_panel::toggle", function(scr)
		if scr == s then
			s.notification_panel.visible = not s.notification_panel.visible
		end
	end)
end
