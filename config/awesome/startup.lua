local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local helpers = require("helpers")

require("awful.autofocus")

client.connect_signal("request::manage", function(client)
	if not awesome.startup then
		client:to_secondary_section()
	end
end)

client.connect_signal("manage", function(c)
	c.shape = helpers.rrect(8)
end)

client.connect_signal("property::floating", function(client)
	if client.floating and not client.fullscreen then
		client.ontop = true
	else
		client.ontop = false
	end
end)

client.connect_signal("property::fullscreen", function(c)
	if c.fullscreen then
		c.shape = gears.shape.rectangle
	else
		c.shape = helpers.rrect(8)
	end
end)

client.connect_signal("mouse::enter", function(client)
	if not client.fullscreen and client.can_focus ~= false then
		client:activate({ context = "mouse_enter", raise = false })
	end
end)

-- Wallpaper
screen.connect_signal("request::wallpaper", function(s)
	local width = s.geometry.width
	if width == 1920 then
		awful.wallpaper({
			screen = s,
			widget = {
				image = gears.filesystem.get_xdg_config_home()
					.. "/wallpapers/2024_Into_the_Light_Press_Kit_Cinematics_COMPRESSED_003.jpg",
				upscale = false,
				downscale = true,
				valign = "center",
				halign = "center",
				horizontal_fit_policy = "fit",
				vertical_fit_policy = "auto",
				widget = wibox.widget.imagebox,
			},
		})
	elseif width == 3440 then
		awful.wallpaper({
			screen = s,
			widget = {
				image = gears.filesystem.get_xdg_config_home() .. "/wallpapers/forest_house.jpg",
				upscale = true,
				downscale = false,
				valign = "center",
				halign = "center",
				horizontal_fit_policy = "auto",
				vertical_fit_policy = "auto",
				widget = wibox.widget.imagebox,
			},
		})
	elseif width == 1080 then
		awful.wallpaper({
			screen = s,
			widget = {
				widget = wibox.container.rotate,
				direction = "west",
				{
					widget = wibox.widget.imagebox,
					image = gears.filesystem.get_xdg_config_home() .. "/wallpapers/cyberpunk_helmet.png",
					upscale = false,
					downscale = true,
					valign = "center",
					halign = "center",
					horizontal_fit_policy = "fit",
					vertical_fit_policy = "auto",
				},
			},
		})
	else
		awful.wallpaper({
			screen = s,
			widget = {
				image = gears.filesystem.get_xdg_config_home() .. "/wallpapers/zaindul_mountains.png",
				upscale = false,
				downscale = true,
				valign = "center",
				halign = "center",
				horizontal_fit_policy = "fit",
				vertical_fit_policy = "auto",
				widget = wibox.widget.imagebox,
			},
		})
	end
end)
