local awful = require("awful")
local ruled = require("ruled")
local utils = require("utils")

ruled.client.connect_signal("request::rules", function()
    -- All clients will match this rule.
    ruled.client.append_rule({
        id = "global",
        rule = {},
        except_any = { class = { "polybar" } },
        properties = {
            focus = awful.client.focus.filter,
            raise = true,
            screen = awful.screen.preferred,
            placement = awful.placement.no_overlap + awful.placement
                .no_offscreen
        }
    })

    -- Floating clients.
    ruled.client.append_rule({
        id = "floating",
        rule_any = {
            type = { "dialog" },
            instance = { "spad" },
            class = {
                "Arandr",
                "Spotify",
                "Mailspring",
                "Slack",
                "Rocket.Chat",
                "discord",
                "gnome-calendar"
            },
            name = {
                "Discord Updater",
                "Steam - Self Updater",
                "Android Emulator*",
                "JetBrains Toolbox"
            },
            role = { "pop-up" }
        },
        properties = { floating = true, placement = utils.placement.centered }
    })

    ruled.client.append_rule({
        id = "calendar",
        rule = { class = "gnome-calendar" },
        properties = {
            floating = true,
            placement = utils.placement.centered,
            width = 640,
            height = 480
        }
    })

    -- Add titlebars to normal clients and dialogs
    ruled.client.append_rule({
        id = "titlebars",
        rule_any = { type = { "normal", "dialog" } },
        properties = { titlebars_enabled = true }
    })

    -- Set Firefox to always map on the tag named "2" on screen 1.
    ruled.client.append_rule {
        rule = { class = "Firefox", "google-chrome" },
        properties = { screen = 2, tag = "1" }
    }
end)
