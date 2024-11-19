local wibox = require("wibox")
local beautiful = require("beautiful")

local wrapper = {}

local function init(content)
	local widget = wibox.widget({
		widget = wibox.container.background,
		bg = beautiful.transparent,
		content,
	})
	return widget
end

return setmetatable(wrapper, {
	__call = function(_, ...)
		return init(...)
	end,
})
