local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
local helpers = require("helpers")

local volume = require("widgets").volume
local clock = require("widgets").clock
local cpu = require("widgets").cpu
local mem = require("widgets").mem
local fs = require("widgets").fs

local notifications = {
	align = "center",
	font = beautiful.icon .. " 16",
	markup = helpers.colorize_text("󰂜", beautiful.fg),
	widget = wibox.widget.textbox,
	-- buttons = {
	-- 	awful.button({}, 1, function()
	-- 		awesome.emit_signal("toggle::notify")
	-- 	end),
	-- },
}

local widget = wibox.widget({
	widget = wibox.container.background,
	shape = helpers.rrect(2),
	bg = beautiful.color0,
	{
		widget = wibox.container.margin,
		margins = dpi(10),
		{
			layout = wibox.layout.fixed.horizontal,
			spacing = dpi(20),
			-- notifications,
			fs(),
			mem(),
			cpu(),
			volume(),
			clock(),
		},
	},
})

return widget
