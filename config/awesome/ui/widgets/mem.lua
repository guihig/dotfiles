local wibox = require("wibox")
local beautiful = require("beautiful")
local wrapper = require("ui.widgets.wrapper")
local lain = require("lain")
local xresources = require("beautiful.xresources")

local dpi = xresources.apply_dpi

local mem = {}

local function init()
	local icon = wibox.widget({
		text = beautiful.icons.mem,
		align = "center",
		valign = "center",
		font = beautiful.icon,
		widget = wibox.widget.textbox,
	})

	local lain_mem = lain.widget.mem({
		settings = function()
			widget:set_text(string.format("%s%%", mem_now.perc))
		end,
	})

	local widget = wibox.widget({
		layout = wibox.layout.fixed.horizontal,
		spacing = dpi(4),
		icon,
		lain_mem.widget,
	})

	return wrapper(widget)
end

return setmetatable(mem, {
	__call = function(_, ...)
		return init(...)
	end,
})
