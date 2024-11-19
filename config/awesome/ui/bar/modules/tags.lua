local awful = require("awful")
local wibox = require("wibox")
local utils = require("utils")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
local rubato = require("modules.rubato")

return function(s)
	local taglist = awful.widget.taglist({
		layout = {
			spacing = 8,
			layout = wibox.layout.fixed.horizontal,
		},
		style = {
			shape = utils.rrect(8),
		},
		screen = s,
		filter = awful.widget.taglist.filter.all,
		buttons = {
			awful.button({}, 1, function(t)
				t:view_only()
			end),
			awful.button({}, 4, function(t)
				awful.tag.viewprev(t.screen)
			end),
			awful.button({}, 5, function(t)
				awful.tag.viewnext(t.screen)
			end),
		},
		widget_template = {
			{
				{
					markup = "",
					shape = utils.rrect(3),
					widget = wibox.widget.textbox,
				},
				valign = "center",
				id = "background_role",
				shape = utils.rrect(1),
				widget = wibox.container.background,
				forced_width = dpi(40),
				forced_height = dpi(14),
			},
			widget = wibox.container.place,
			create_callback = function(self, tag)
				self.tag_animation = rubato.timed({
					duration = 0.25,
					easing = rubato.linear,
					subscribed = function(pos)
						self:get_children_by_id("background_role")[1].forced_width = pos
					end,
				})
				self.update = function()
					if tag.selected then
						self.tag_animation.target = dpi(60)
					elseif #tag:clients() > 0 then
						self.tag_animation.target = dpi(40)
					else
						self.tag_animation.target = dpi(20)
					end
				end
				self.update()
			end,
			update_callback = function(self)
				self.update()
			end,
		},
	})
	local wrapper = {
		widget = wibox.container.background,
		shape = utils.rrect(4),
		bg = beautiful.color0,
		{
			widget = wibox.container.margin,
			left = dpi(12),
			right = dpi(12),
			taglist,
		},
	}
	return wrapper
end
