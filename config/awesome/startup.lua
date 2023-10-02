local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")

require("awful.autofocus")

client.connect_signal("request::manage", function(client)
    if not awesome.startup then client:to_secondary_section() end
end)

client.connect_signal("manage",
                      function(c) c.shape = gears.shape.rounded_rect end)

client.connect_signal("property::floating", function(client)
    if client.floating and not client.fullscreen then
        client.ontop = true
    else
        client.ontop = false
    end
end)

client.connect_signal("mouse::enter", function(client)
    if not client.fullscreen and client.can_focus ~= false then
        client:activate{context = "mouse_enter", raise = false}
    end
end)

-- Wallpaper
screen.connect_signal("request::wallpaper", function(s)
    awful.wallpaper {
        screen = s,
        widget = {
            image = gears.filesystem.get_xdg_config_home() ..
                "/wallpapers/flatppuccin_4k_macchiato.png",
            upscale = false,
            downscale = true,
            valign = "center",
            halign = "center",
            horizontal_fit_policy = "fit",
            vertical_fit_policy = "auto",
            widget = wibox.widget.imagebox
        }
    }
end)

-- Screen ordering
-- screen[2]:swap(screen[1])
