local wibox = require("wibox")
local naughty = require("naughty")
local beautiful = require("beautiful")
local helpers = require("helpers")
local inspect = require("inspect")
local dpi = beautiful.xresources.apply_dpi

local empty_widget = require("ui.panels.notifications.core.empty")
local scroller = require("ui.panels.notifications.core.scroller")

local M = {}

M.remove_empty = true

M.layout = wibox.widget({
	layout = wibox.layout.fixed.vertical,
	spacing = dpi(10),
	empty_widget,
})

scroller(M.layout)

M.reset_layout = function()
	M.layout:reset()
	M.layout:insert(1, empty_widget)
	M.remove_empty = true
end

local add_notification = function(n, icon, color)
	if #M.layout.children == 1 and M.remove_empty then
		M.layout:reset(M.layout)
		M.remove_empty = false
	end

	local generator = require("ui.panels.notifications.core.generator")
	M.layout:insert(1, generator(n, icon, n.title, n.message, n.app_name, color, M.layout, M.reset_layout))
end

naughty.connect_signal("request::display", function(n)
	if helpers.misc.table_includes({ "lain_popup" }, n.category) then
		return
	end

	local color = beautiful.transparent
	if n.urgency == "critical" then
		color = n.bg .. "66"
	end

	local icon = n.icon or n.app_icon
	if not icon then
		icon = beautiful.theme_assets.awesome_icon(24, beautiful.color8, beautiful.color3)
	end

	add_notification(n, icon, color)
end)

return M
