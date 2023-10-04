local gobject = require("gears.object")
local gtable = require("gears.table")
local lain = require("lain")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local wibox = require("wibox")

local dpi = xresources.apply_dpi

local instance = nil

local icon = wibox.widget({
	text = beautiful.icons.volume,
	align = "center",
	valign = "center",
	font = beautiful.icon_font,
	widget = wibox.widget.textbox,
})

local audio = lain.widget.alsabar({
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

local function new()
	local ret = gobject({})
	gtable.crush(ret, audio, true)
	ret.icon = icon
	return ret
end

if not instance then
	instance = new()
end

return instance
