local awful = require("awful")
local wibox = require("wibox")
local xresources = require("beautiful.xresources")
local gears = require("gears")
local spawn = require("awful.spawn")
local beautiful = require("beautiful")
local lain = require("lain")
local wrapper = require("widgets.wrapper")

local dpi = xresources.apply_dpi

local volume = {}

local function init()
	local icon = wibox.widget({
		text = beautiful.icons.volume,
		align = "center",
		valign = "center",
		font = beautiful.icon_font,
		widget = wibox.widget.textbox,
	})

	local alsabar = lain.widget.alsabar({
		width = dpi(60),
		border_width = 0,
		ticks = true,
		ticks_size = dpi(6),
		notification_preset = { font = beautiful.font },
		settings = function()
			if volume_now.status == "off" then
				icon:set_markup_silently(beautiful.icons.volume_mute)
			elseif volume_now.level == 0 then
				icon:set_markup_silently(beautiful.icons.volume_off)
			elseif volume_now.level <= 50 then
				icon:set_markup_silently(beautiful.icons.volume_low)
			else
				icon:set_markup_silently(beautiful.icons.volume)
			end
		end,
		colors = {
			background = beautiful.bg_normal,
			mute = beautiful.bg_urgent,
			unmute = beautiful.fg_normal,
		},
	})

	alsabar.bar:buttons(awful.util.table.join(
		awful.button({}, 1, function() -- left click
			awful.spawn(string.format("%s -e alsamixer", "alacritty"))
		end),
		awful.button({}, 2, function() -- middle click
			os.execute(string.format("%s set %s 85%%", alsabar.cmd, alsabar.channel))
			alsabar.update()
		end),
		awful.button({}, 3, function() -- right click
			os.execute(string.format("%s set %s toggle", alsabar.cmd, alsabar.togglechannel or alsabar.channel))
			alsabar.update()
		end),
		awful.button({}, 4, function() -- scroll up
			os.execute(string.format("%s set %s 5%%+", alsabar.cmd, alsabar.channel))
			alsabar.update()
		end),
		awful.button({}, 5, function() -- scroll down
			os.execute(string.format("%s set %s 5%%-", alsabar.cmd, alsabar.channel))
			alsabar.update()
		end)
	))

	local widget = wibox.widget({
		layout = wibox.layout.fixed.horizontal,
		icon,
		alsabar.bar,
	})
	return wrapper(widget)
end

return setmetatable(volume, {
	__call = function(_, ...)
		return init(...)
	end,
})
