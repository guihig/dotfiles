local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
local utils = require("utils")

local tasklist_buttons = gears.table.join(
	-- Left click
	awful.button({}, 1, function(c)
		if c == client.focus then
			c.minimized = true
		else
			c:emit_signal("request::activate", "tasklist", { raise = true })
		end
	end)
)

return function(s)
	local task = wibox.widget({
		widget = awful.widget.tasklist({
			screen = s,
			filter = awful.widget.tasklist.filter.currenttags,
			buttons = tasklist_buttons,
			layout = {
				layout = wibox.layout.fixed.horizontal,
			},
			style = {
				shape = utils.rrect(1),
			},
			widget_template = {
				nil,
				{
					{
						widget = wibox.container.margin,
						margins = dpi(5),
						{
							id = "clienticon",
							widget = awful.widget.clienticon,
						},
					},
					halign = "center",
					valign = "center",
					widget = wibox.container.place,
				},
				{
					wibox.widget.base.make_widget(),
					forced_height = dpi(4),
					id = "background_role",
					widget = wibox.container.background,
				},
				create_callback = function(self, c, _, _)
					self:get_children_by_id("clienticon")[1].client = c
				end,
				layout = wibox.layout.align.vertical,
			},
		}),
	})

	local wrapper = {
		widget = wibox.container.background,
		shape = utils.rrect(4),
		bg = beautiful.color0,
		{
			widget = wibox.container.margin,
			left = dpi(4),
			right = dpi(4),
			task,
		},
	}
	return wrapper
end
