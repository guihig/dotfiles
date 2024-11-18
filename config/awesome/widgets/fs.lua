local wibox = require("wibox")
local beautiful = require("beautiful")
local wrapper = require("widgets.wrapper")
local lain = require("lain")
local xresources = require("beautiful.xresources")

local dpi = xresources.apply_dpi

local fs = {}

local function init()
	local icon = wibox.widget({
		text = beautiful.icons.fs,
		align = "center",
		valign = "center",
		font = beautiful.icon,
		widget = wibox.widget.textbox,
	})

	local lain_fs = lain.widget.fs({
		notification_preset = { font = beautiful.font },
		settings = function()
			widget:set_text(string.format("%s%%", fs_now["/"].percentage))
		end,
	})

	local widget = wibox.widget({
		layout = wibox.layout.fixed.horizontal,
		spacing = dpi(4),
		icon,
		lain_fs.widget,
	})

	return wrapper(widget)
end

return setmetatable(fs, {
	__call = function(_, ...)
		return init(...)
	end,
})
