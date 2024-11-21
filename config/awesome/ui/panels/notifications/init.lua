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
			align = "center",
			valign = "center",
			widget = wibox.widget.textbox,
		},
		layout = wibox.layout.fixed.horizontal,
	})

	local clear_btn = wibox.widget({
		{
			icon,
			margins = dpi(12),
			widget = wibox.container.margin,
		},
		bg = beautiful.notification_panel_bg_alt,
		shape = helpers.ui.rrect(beautiful.border_radius),
		widget = wibox.container.background,
	})

	clear_btn:buttons(awful.util.table.join(awful.button({}, 1, function()
		awesome.emit_signal("notification_panel::clear")
	end)))

	helpers.ui.add_hover_cursor(clear_btn, "hand2")

	return clear_btn
end

return function(s)
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
					helpers.ui.vertical_pad(dpi(20)),
					{
						require("ui.panels.notifications.core")(s),
						margins = dpi(20),
						widget = wibox.container.margin,
					},
					layout = wibox.layout.fixed.vertical,
				},
				{
					helpers.ui.vertical_pad(dpi(20)),
					{
						clear_button(),
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
