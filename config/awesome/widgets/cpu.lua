local wibox = require("wibox")
local beautiful = require("beautiful")
local wrapper = require("widgets.wrapper")
local lain = require("lain")
local xresources = require("beautiful.xresources")

local dpi = xresources.apply_dpi

local cpu = {}

local function init()
	local icon = wibox.widget({
		text = beautiful.icons.cpu,
		align = "center",
		valign = "center",
		font = beautiful.icon,
		widget = wibox.widget.textbox,
	})

	local lain_cpu = lain.widget.cpu({
		settings = function()
			widget:set_text(string.format("%s%%", cpu_now.usage))
		end,
	})

	local widget = wibox.widget({
		layout = wibox.layout.fixed.horizontal,
		spacing = dpi(4),
		icon,
		lain_cpu.widget,
	})

	return wrapper(widget)
end

return setmetatable(cpu, {
	__call = function(_, ...)
		return init(...)
	end,
})
