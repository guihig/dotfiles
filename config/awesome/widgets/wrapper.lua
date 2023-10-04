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
			margins = {
				top = dpi(2),
				bottom = dpi(2),
				left = dpi(2),
				right = dpi(4),
			},
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
