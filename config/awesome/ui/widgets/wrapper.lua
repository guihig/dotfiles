local wibox = require("wibox")
local beautiful = require("beautiful")
local utils = require("utils")

local wrapper = {}

local function init(content)
	local widget = wibox.widget({
		widget = wibox.container.background,
		bg = beautiful.transparent,
		content,
	})
	utils.ui.add_hover_cursor(widget, "hand2")
	return widget
end

return setmetatable(wrapper, {
	__call = function(_, ...)
		return init(...)
	end,
})
