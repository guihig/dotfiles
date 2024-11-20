local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local wrapper = require("ui.widgets.wrapper")
local xresources = require("beautiful.xresources")

local dpi = xresources.apply_dpi

local clock = {}

local function init()
	local icon = wibox.widget({
		text = beautiful.icons.bell,
		align = "center",
		valign = "center",
		font = beautiful.icon .. "12",
		widget = wibox.widget.textbox,
		buttons = {
			awful.button({}, 1, function()
				awesome.emit_signal("notification_panel::toggle", awful.screen.focused())
			end),
		},
	})

	local widget = wibox.widget({
		layout = wibox.layout.fixed.horizontal,
		spacing = dpi(4),
		icon,
	})

	return wrapper(widget)
end

return setmetatable(clock, {
	__call = function(_, ...)
		return init(...)
	end,
})
