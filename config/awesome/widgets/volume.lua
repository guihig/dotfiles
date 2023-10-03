local awful = require("awful")
local wibox = require("wibox")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local gears = require("gears")
local watch = require("awful.widget.watch")
local spawn = require("awful.spawn")
local rubato = require("modules.rubato")
local beautiful = require("beautiful")
local cjson = require("cjson")

local volume = {}
local devices_rows = { layout = wibox.layout.fixed.vertical }
local devices_popup = awful.popup({
	bg = beautiful.bg_normal,
	ontop = true,
	visible = false,
	shape = gears.shape.rounded_rect,
	border_width = 1,
	border_color = beautiful.bg_focus,
	maximum_width = 400,
	offset = { y = 5 },
	widget = {},
})

local LIST_DEVICES_CMD = "pactl -f json list sources"
local GET_VOL_CMD = "amixer -D default sget Master"
local INC_VOL_CMD = "amixer -D default sset Master %s%%+"
local DEC_VOL_CMD = "amixer -D default sset Master %s%%-"
local TOGGLE_VOL_CMD = "pactl set-sink-mute @DEFAULT_SINK@ toggle"

local function build_header_row(text)
	return wibox.widget({
		{
			markup = "<b>" .. text .. "</b>",
			align = "center",
			widget = wibox.widget.textbox,
		},
		bg = beautiful.bg_normal,
		widget = wibox.container.background,
	})
end

local function build_rows(devices, on_checkbox_click, device_type)
	local device_rows = { layout = wibox.layout.fixed.vertical }
	for _, device in pairs(devices) do
		local checkbox = wibox.widget({
			checked = device.is_default,
			color = beautiful.bg_normal,
			paddings = 2,
			shape = gears.shape.circle,
			forced_width = 20,
			forced_height = 20,
			check_color = beautiful.fg_urgent,
			widget = wibox.widget.checkbox,
		})

		checkbox:connect_signal("button::press", function()
			spawn.easy_async(string.format([[sh -c 'pactl set-default-%s "%s"']], device_type, device.id), function()
				on_checkbox_click()
			end)
		end)

		local row = wibox.widget({
			{
				{
					{
						checkbox,
						valign = "center",
						layout = wibox.container.place,
					},
					{
						{
							text = device["name"],
							align = "left",
							widget = wibox.widget.textbox,
						},
						left = 10,
						layout = wibox.container.margin,
					},

					spacing = 8,
					layout = wibox.layout.align.horizontal,
				},
				margins = 4,
				layout = wibox.container.margin,
			},
			bg = beautiful.bg_normal,
			widget = wibox.container.background,
		})

		row:connect_signal("mouse::enter", function(c)
			c:set_bg(beautiful.bg_focus)
		end)
		row:connect_signal("mouse::leave", function(c)
			c:set_bg(beautiful.bg_normal)
		end)

		local old_cursor, old_wibox
		row:connect_signal("mouse::enter", function()
			local wb = mouse.current_wibox
			old_cursor, old_wibox = wb.cursor, wb
			wb.cursor = "hand1"
		end)
		row:connect_signal("mouse::leave", function()
			if old_wibox then
				old_wibox.cursor = old_cursor
				old_wibox = nil
			end
		end)

		row:connect_signal("button::press", function()
			spawn.easy_async(string.format([[sh -c 'pactl set-default-%s "%s"']], device_type, device.id), function()
				on_checkbox_click()
			end)
		end)

		table.insert(device_rows, row)
	end

	return device_rows
end

local function show_devices_popup()
	spawn.easy_async(LIST_DEVICES_CMD, function(stdout)
		local devices = cjson.decode(stdout)
		local sinks = {}
		local sources = {}

		local default_sink = io.popen("pactl get-default-sink"):read("*a")
		default_sink = default_sink:gsub("[\r\n]", "")
		local default_source = io.popen("pactl get-default-source"):read("*a")
		default_source = default_source:gsub("[\r\n]", "")

		for _key, device in pairs(devices) do
			if device["properties"]["media.class"] == "Audio/Sink" then
				table.insert(sinks, {
					id = device["index"],
					name = device["properties"]["alsa.card_name"],
					is_default = device["properties"]["node.name"] == default_sink,
				})
			else
				table.insert(sources, {
					id = device["index"],
					name = device["properties"]["alsa.card_name"],
					is_default = device["properties"]["node.name"] == default_source,
				})
			end
		end

		for i = 0, #devices_rows do
			devices_rows[i] = nil
		end

		table.insert(devices_rows, build_header_row("SINKS"))
		table.insert(
			devices_rows,
			build_rows(sinks, function()
				show_devices_popup()
			end, "sink")
		)
		table.insert(devices_rows, build_header_row("SOURCES"))
		table.insert(
			devices_rows,
			build_rows(sources, function()
				show_devices_popup()
			end, "source")
		)

		devices_popup:setup(devices_rows)
	end)
end

local function init()
	local refresh_rate = 1
	local default_step = 5

	local vol_bar = wibox.widget({
		max_value = 100,
		value = 0,
		forced_height = dpi(4),
		forced_width = 100,
		paddings = 1,
		shape = gears.shape.rounded_bar,
		widget = wibox.widget.progressbar,
		margins = 5,
		background_color = beautiful.bg_focus,
		color = beautiful.fg,
	})

	volume.widget = wibox.widget({ layout = wibox.layout.stack, vol_bar })
	volume.muted = false

	local bar_anim = rubato.timed({
		duration = 0.125,
		subscribed = function(pos)
			vol_bar.value = pos
		end,
	})

	local function update_bar(_, stdout)
		bar_anim.pos = vol_bar.value or 0

		local vol_level = string.match(stdout, "%[(%d?%d?%d?)%%%]")
		local vol_value = tonumber(vol_level)
		if not vol_value then
			print("err parsing vol level")
			return
		end

		bar_anim.target = vol_value
	end

	function volume:inc(s)
		local cmd = string.format(INC_VOL_CMD, s or default_step)
		spawn.easy_async(cmd, function(stdout)
			update_bar(volume.widget, stdout)
		end)
	end

	function volume:dec(s)
		local cmd = string.format(DEC_VOL_CMD, s or default_step)
		spawn.easy_async(cmd, function(stdout)
			update_bar(volume.widget, stdout)
		end)
	end

	function volume:toggle()
		spawn.easy_async(TOGGLE_VOL_CMD, function()
			volume.muted = not volume.muted
			if volume.muted then
				vol_bar.background_color = beautiful.red
				vol_bar.color = beautiful.red
			else
				vol_bar.background_color = beautiful.bg_focus
				vol_bar.color = beautiful.fg
			end
		end)
	end

	volume.widget:buttons(gears.table.join(
		awful.button({}, 4, function()
			volume:inc()
		end),
		awful.button({}, 5, function()
			volume:dec()
		end),
		awful.button({}, 1, function()
			volume:toggle()
		end),
		awful.button({}, 3, function()
			if devices_popup.visible then
				devices_popup.visible = not devices_popup.visible
			else
				show_devices_popup()
				devices_popup:move_next_to(mouse.current_widget_geometry)
			end
		end)
	))

	watch(GET_VOL_CMD, refresh_rate, update_bar, volume.widget)
	return volume.widget
end

return setmetatable(volume, {
	__call = function(_, ...)
		return init(...)
	end,
})
