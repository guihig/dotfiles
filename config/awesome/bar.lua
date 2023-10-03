local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local beautiful = require("beautiful")
local utils = require("utils")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

-- Widgets
local volume = require("widgets").volume
local clock = require("widgets").clock

-- Task list
local function tasklist(s)
	local _tasklist = awful.widget.tasklist({
		screen = s,
		filter = awful.widget.tasklist.filter.focused,
		style = {

			shape = gears.shape.rounded_rect,
		},
		layout = {
			spacing = 5,
			forced_num_rows = 1,
			layout = wibox.layout.grid.horizontal,
		},
		widget_template = {
			{
				{
					{
						{
							{ id = "icon_role", widget = wibox.widget.imagebox },
							top = 3,
							bottom = 3,
							left = 3,
							right = 5,
							widget = wibox.container.margin,
						},
						{
							{ id = "text_role", widget = wibox.widget.textbox },
							margins = 3,
							widget = wibox.container.margin,
						},
						layout = wibox.layout.fixed.horizontal,
					},
					left = 5,
					right = 5,
					widget = wibox.container.margin,
				},
				id = "background_role",
				widget = wibox.container.background,
			},
			margins = 4,
			widget = wibox.container.margin,
		},
	})

	return _tasklist
end

-- Tag list
local function taglist(s)
	local taglist_buttons = gears.table.join(awful.button({}, 1, function(t)
		t:view_only()
	end))

	local _taglist = awful.widget.taglist({
		screen = s,
		filter = awful.widget.taglist.filter.all,
		buttons = taglist_buttons,
	})

	return _taglist
end

-- Layout box
local function layout_box(s)
	local layout_box_buttons = gears.table.join(
		awful.button({}, 1, function()
			awful.layout.inc(1)
			utils.handle_master_count()
		end),
		awful.button({}, 3, function()
			awful.layout.inc(-1)
			utils.handle_master_count()
		end)
	)

	local _layout_box = awful.widget.layoutbox({
		screen = s,
		buttons = layout_box_buttons,
		forced_height = 16,
		forced_width = 16,
	})

	return _layout_box
end

-- Systray
local function systray()
	local _systray = wibox.widget.systray()

	_systray.set_base_size(16)

	return _systray
end

awful.screen.connect_for_each_screen(function(s)
	-- Create a promptbox for each screen
	s.mypromptbox = awful.widget.prompt()

	-- Layout box
	s.mylayoutbox = layout_box(s)

	-- Tag list
	s.mytaglist = taglist(s)

	-- Task list
	s.mytasklist = tasklist(s)

	-- Systray
	s.mysystray = systray()

	-- Create the wibox
	s.mywibox = awful.wibar({
		position = "top",
		screen = s,
		widget = {
			layout = wibox.layout.align.horizontal,
			{
				layout = wibox.layout.fixed.horizontal,
				s.mytaglist,
				s.mytasklist,
			},
			{
				layout = wibox.layout.flex.horizontal,
			},
			{
				{
					layout = wibox.layout.fixed.horizontal,
					spacing = 5,
					-- TODO: Add mem, fs, cpu
					volume(),
					clock(),
					s.mysystray,
					layout_box(s),
				},
				widget = wibox.container.margin,
				margins = 5,
			},
		},
	})
end)
