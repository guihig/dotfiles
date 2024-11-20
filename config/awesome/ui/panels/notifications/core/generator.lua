local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi

local builder = require("ui.panels.notifications.core.builder")

local return_date_time = function(format)
	return os.date(format)
end

local parse_to_seconds = function(time)
	local hourInSec = tonumber(string.sub(time, 1, 2)) * 3600
	local minInSec = tonumber(string.sub(time, 4, 5)) * 60
	local getSec = tonumber(string.sub(time, 7, 8))
	return (hourInSec + minInSec + getSec)
end

local generator = function(notification, icon, title, message, app, bgcolor, inbox_layout, inbox_reset_layout)
	local time_of_pop = return_date_time("%H:%M:%S")
	local exact_time = return_date_time("%I:%M %p")
	local exact_date_time = return_date_time("%b %d, %I:%M %p")

	local timepop = wibox.widget({
		id = "time_pop",
		markup = nil,
		font = beautiful.font .. "Regular 10",
		align = "left",
		valign = "center",
		visible = true,
		widget = wibox.widget.textbox,
	})

	local dismiss = builder.dismiss()

	gears.timer({
		timeout = 60,
		call_now = true,
		autostart = true,
		callback = function()
			local time_difference = nil

			time_difference = parse_to_seconds(return_date_time("%H:%M:%S")) - parse_to_seconds(time_of_pop)
			time_difference = tonumber(time_difference)

			if time_difference < 60 then
				timepop:set_markup("now")
			elseif time_difference >= 60 and time_difference < 3600 then
				local time_in_minutes = math.floor(time_difference / 60)
				timepop:set_markup(time_in_minutes .. "m ago")
			elseif time_difference >= 3600 and time_difference < 86400 then
				timepop:set_markup(exact_time)
			elseif time_difference >= 86400 then
				timepop:set_markup(exact_date_time)
				return false
			end

			collectgarbage("collect")
		end,
	})

	local notification_template = wibox.widget({
		id = "notification_template",
		expand = "none",
		{
			{
				layout = wibox.layout.fixed.vertical,
				spacing = dpi(5),
				{
					expand = "none",
					layout = wibox.layout.align.horizontal,
					{
						layout = wibox.layout.fixed.horizontal,
						spacing = dpi(5),
						builder.icon(icon),
						builder.appname(app),
					},
					nil,
					{
						timepop,
						dismiss,
						layout = wibox.layout.fixed.horizontal,
					},
				},
				{
					layout = wibox.layout.fixed.vertical,
					spacing = dpi(5),
					{
						builder.title(title),
						builder.message(message),
						layout = wibox.layout.fixed.vertical,
					},
					builder.actions(notification),
				},
			},
			margins = dpi(10),
			widget = wibox.container.margin,
		},
		bg = bgcolor,
		shape = function(cr, width, height)
			gears.shape.partially_rounded_rect(cr, width, height, true, true, true, true, beautiful.border_radius)
		end,
		widget = wibox.container.background,
	})

	--- Put the generated template to a container
	local notification_box = wibox.widget({
		notification_template,
		shape = function(cr, width, height)
			gears.shape.partially_rounded_rect(cr, width, height, true, true, true, true, beautiful.border_radius)
		end,
		widget = wibox.container.background,
	})

	--- Delete notification box
	local notification_delete = function()
		inbox_layout:remove_widgets(notification_box, true)
	end

	--- Delete notification_box on LMB
	notification_box:buttons(awful.util.table.join(awful.button({}, 1, function()
		if #inbox_layout.children == 1 then
			inbox_reset_layout()
		else
			notification_delete()
		end
		collectgarbage("collect")
	end)))

	--- Add hover, and mouse leave events
	notification_template:connect_signal("mouse::enter", function()
		notification_box.bg = beautiful.notification_panel_bg_alt
		timepop.visible = false
		dismiss.visible = true
	end)

	notification_template:connect_signal("mouse::leave", function()
		notification_box.bg = beautiful.transparent
		timepop.visible = true
		dismiss.visible = false
	end)

	collectgarbage("collect")

	return notification_box
end

return generator
