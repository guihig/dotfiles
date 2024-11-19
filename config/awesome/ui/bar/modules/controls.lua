local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
local utils = require("utils")

local volume = require("ui.widgets").volume
local clock = require("ui.widgets").clock
local cpu = require("ui.widgets").cpu
local mem = require("ui.widgets").mem
local fs = require("ui.widgets").fs

local notifications = {
	align = "center",
	font = beautiful.icon .. " 16",
	markup = utils.colorize_text("󰂜", beautiful.fg),
	widget = wibox.widget.textbox,
	-- buttons = {
	-- 	awful.button({}, 1, function()
	-- 		awesome.emit_signal("toggle::notify")
	-- 	end),
	-- },
}

local widget = wibox.widget({
	widget = wibox.container.background,
	shape = utils.rrect(2),
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
