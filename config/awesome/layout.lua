local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local bling = require("modules.bling")

tag.connect_signal("request::default_layouts", function()
    awful.layout.append_default_layouts({
        awful.layout.suit.tile,
        awful.layout.suit.max,
        bling.layout.equalarea
    })
end)

-- Wallpaper
screen.connect_signal("request::wallpaper", function(s)
    awful.wallpaper {
        screen = s,
        widget = {
            image = HOME_DIR
                .. "/dotfiles/wallpapers/flatppuccin_4k_macchiato.png",
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

-- Titlebars
client.connect_signal("request::titlebars", function(c)
    awful.titlebar(c).widget = {
        { -- Left
            {
                awful.titlebar.widget.iconwidget(c),
                layout = wibox.layout.fixed.horizontal
            },
            widget = wibox.container.margin,
            margins = 5
        },
        { -- Middle
            { -- Title
                halign = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            layout = wibox.layout.flex.horizontal
        },
        { -- Right
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Rounded borders
client.connect_signal("manage",
                      function(c) c.shape = gears.shape.rounded_rect end)

-- Window Focus
client.connect_signal("mouse::enter", function(c)
    c:activate({ context = "mouse_enter", raise = false })
end)

-- Screen ordering
screen[2]:swap(screen[1])
