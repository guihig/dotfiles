local wibox = require("wibox")
local beautiful = require("beautiful")
local wrapper = require("ui.widgets.wrapper")
local lain = require("lain")
local xresources = require("beautiful.xresources")

local dpi = xresources.apply_dpi

local clock = {}

local function init()
	local icon = wibox.widget({
		text = beautiful.icons.calendar,
		align = "center",
		valign = "center",
		font = beautiful.icon .. "12",
		widget = wibox.widget.textbox,
	})

	lain.widget.cal({
		attach_to = { icon },
		notification_preset = {
			font = beautiful.font .. "Medium 9",
			category = "cal",
			bg = beautiful.notification_bg,
			fg = beautiful.notification_fg,
		},
	})

	local clock_text = wibox.widget({
		widget = wibox.widget.textclock,
		format = "%H:%M",
		valign = "center",
	})

	clock_text:connect_signal("button::press", function(self, _lx, _ly, button, _mods, _find_widget_results)
		local short_format = "%H:%M"
		local long_format = "%d/%m/%Y %H:%M"

		if button == 1 then
			if self.format == long_format then
				self.format = short_format
			else
				self.format = long_format
			end
		end
	end)

	local widget = wibox.widget({
		layout = wibox.layout.fixed.horizontal,
		spacing = dpi(4),
		icon,
		clock_text,
	})

	return wrapper(widget)
end

return setmetatable(clock, {
	__call = function(_, ...)
		return init(...)
	end,
})
