local awful = require("awful")
local ruled = require("ruled")
local utils = require("utils")

require("awful.autofocus")

client.connect_signal("request::manage", function(client)
    if not awesome.startup then client:to_secondary_section() end
end)

client.connect_signal("property::floating", function(client)
    if client.floating and not client.fullscreen then
        client.ontop = true
    else
        client.ontop = false
    end
end)

client.connect_signal("mouse::enter", function(client)
    if not client.fullscreen and client.can_focus ~= false then
        client:activate{ context = "mouse_enter", raise = false }
    end
end)

ruled.client.connect_signal("request::rules", function()
    -- Global
    ruled.client.append_rule {
        rule = {},
        properties = {
            focus = awful.client.focus.filter,
            raise = true,
            screen = awful.screen.focused,
            size_hints_honor = false,
            honor_workarea = true,
            honor_padding = true,
            placement = awful.placement.no_overlap + awful.placement
                .no_offscreen
        }
    }

    -- Add titlebars to normal clients and dialogs
    ruled.client.append_rule({
        id = "titlebars",
        rule_any = { type = { "normal", "dialog" } },
        properties = { titlebars_enabled = true }
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

    -- Set Firefox to always map on the tag named "1" on screen 1.
    ruled.client.append_rule {
        rule = { class = "firefox" },
        properties = {
            tag = screen[1].tags[1],
            switch_to_tags = true,
            opacity = 1,
            maximized = false,
            floating = false,
            sticky = false
        }
    }

    ruled.client.append_rule {
        rule = { class = "google-chrome" },
        properties = {
            tag = screen[1].tags[2],
            switch_to_tags = true,
            opacity = 1,
            maximized = false,
            floating = false,
            sticky = false
        }
    }
end)
