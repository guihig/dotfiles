local wibox = require("wibox")
local beautiful = require("beautiful")
local gears = require("gears")
local xresources = require("beautiful.xresources")

local dpi = xresources.apply_dpi

local wrapper = {}

local function init(content)
	local widget = wibox.widget({
		widget = wibox.container.background,
		bg = beautiful.bg_focus,
		shape = gears.shape.octogon,
		{
			layout = wibox.container.margin,
			margins = dpi(2),
			content,
		},
	})
	return widget
end

return setmetatable(wrapper, {
	__call = function(_, ...)
		return init(...)
	end,
})
