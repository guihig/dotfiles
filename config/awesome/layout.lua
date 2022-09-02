local awful = require("awful")
local wibox = require("wibox")

tag.connect_signal("request::default_layouts", function()
    awful.layout.append_default_layouts({
        awful.layout.suit.tile,
        awful.layout.suit.max,
        awful.layout.suit.magnifier
    })
end)

-- Wallpaper
screen.connect_signal("request::wallpaper", function(s)
    awful.wallpaper {
        screen = s,
        widget = {
            {
                image = HOME_DIR .. "/dotfiles/wallpapers/wallpaper.jpg",
                upscale = true,
                downscale = true,
                widget = wibox.widget.imagebox
            },
            valign = "center",
            halign = "center",
            tiled = false,
            widget = wibox.container.tile
        }
    }
end)

-- Titlebars
client.connect_signal("request::titlebars", function(c)
    awful.titlebar(c).widget = {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            layout = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align = "center",
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
