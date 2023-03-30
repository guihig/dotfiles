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
                "spotify",
                "Mailspring",
                "Slack",
                "Rocket.Chat",
                "gnome-calendar"
            },
            name = {
                "Steam - Self Updater",
                "Android Emulator*",
                "JetBrains Toolbox",
                "Discord Updater",
                "splash"
            },
            role = { "pop-up" }
        },
        properties = { floating = true, placement = utils.placement.centered }
    })

    ruled.client.append_rule({
        id = "discord",
        rule_any = { class = { "discord" } },
        except_any = { name = "Discord Updater" },
        properties = { floating = true, placement = awful.placement.left }
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
        rule = { class = "firefox" },
        properties = {
            screen = 1,
            tag = "1",
            opacity = 1,
            maximized = false,
            floating = false,
            sticky = false
        }
    }
end)
