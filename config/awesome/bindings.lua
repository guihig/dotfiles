local awful = require("awful")
local bling = require("bling")
local apps = require("apps")
local scratchpad = require("scratchpad")

local directions = {
    { k = "h", d = "left" },
    { k = "l", d = "right" },
    { k = "k", d = "up" },
    { k = "j", d = "down" }
}

-- General keys
awful.keyboard.append_global_keybindings({
    awful.key({ Modkey }, "q", awesome.restart,
              { description = "reload awesome", group = "awesome" }),
    awful.key({ Modkey, "Shift" }, "q", awesome.quit,
              { description = "quit awesome", group = "awesome" }),
    awful.key({ Modkey }, "Return", function() awful.spawn(apps.terminal) end,
              { description = "open a terminal", group = "launcher" }),
    awful.key({ Modkey }, "space", function() awful.spawn(apps.launcher) end,
              { description = "open a launcher", group = "launcher" }),
    awful.key({ Modkey, "Shift" }, "=",
              function() awful.spawn.with_shell(apps.notification_center) end, {
        description = "open the notification center",
        group = "launcher"
    }),
    awful.key({ Modkey }, "c", function()
        awful.spawn.with_shell(
            "~/.local/bin/notify-send.py a --hint boolean:deadd-notification-center:true string:type:clearPopups")
    end, { description = "clear notifications", group = "util" }),
    awful.key({ Modkey }, "=", function()
        awful.spawn(HOME_DIR .. "/dotfiles/scripts/lock.sh")
    end, { description = "lock screen", group = "util" }),
    awful.key({ "Shift" }, "Print", function() awful.spawn(apps.flameshot) end,
              { description = "open the flameshot gui", group = "launcher" })
})

-- Focus keybindings
local focus_keybindings = {
    awful.key({ Modkey }, ";", function() awful.screen.focus_relative(1) end,
              { description = "focus the other screen", group = "screen" })
}

for _, value in ipairs(directions) do
    table.insert(focus_keybindings, awful.key({ Modkey }, value.k, function()
        awful.client.focus.global_bydirection(value.d, client.focus, false)
        bling.module.flash_focus.flashfocus(client.focus)

    end, { description = "focus " .. value.d .. " client", group = "client" }))
end

awful.keyboard.append_global_keybindings(focus_keybindings)

-- Swap keybindings
local swap_keybindings = {}

for _, value in ipairs(directions) do
    table.insert(swap_keybindings, awful.key({ Modkey, "Control" }, value.k,
                                             function()
        awful.client.swap.bydirection(value.d)
    end, { description = "swap " .. value.d .. " client", group = "client" }))
end

awful.keyboard.append_global_keybindings(swap_keybindings)

-- Layout related keybindings
awful.keyboard.append_global_keybindings({
    awful.key({ Modkey }, "u", awful.client.urgent.jumpto,
              { description = "jump to urgent client", group = "client" }),
    awful.key({ Modkey }, "]", function() awful.tag.incmwfact(0.05) end, {
        description = "increase master width factor",
        group = "layout"
    }),
    awful.key({ Modkey }, "[", function() awful.tag.incmwfact(-0.05) end, {
        description = "decrease master width factor",
        group = "layout"
    }),
    awful.key({ Modkey, "Shift" }, "[",
              function() awful.client.incwfact(-0.05, client.focus) end, {
        description = "decrease client width factor",
        group = "layout"
    }),
    awful.key({ Modkey, "Shift" }, "]",
              function() awful.client.incwfact(0.05, client.focus) end, {
        description = "increase client width factor",
        group = "layout"
    }),
    awful.key({ Modkey }, "Tab", function() awful.layout.inc(1) end,
              { description = "select next", group = "layout" }),
    awful.key({ Modkey, "Shift" }, "Tab", function() awful.layout.inc(-1) end,
              { description = "select previous", group = "layout" })
})

-- @DOC_NUMBER_KEYBINDINGS@
awful.keyboard.append_global_keybindings({
    awful.key({
        modifiers = { Modkey },
        keygroup = "numrow",
        description = "only view tag",
        group = "tag",
        on_press = function(index)
            local screen = awful.screen.focused()
            local tag = screen.tags[index]
            if tag then tag:view_only() end
        end
    }),
    awful.key({
        modifiers = { Modkey, "Control" },
        keygroup = "numrow",
        description = "toggle tag",
        group = "tag",
        on_press = function(index)
            local screen = awful.screen.focused()
            local tag = screen.tags[index]
            if tag then awful.tag.viewtoggle(tag) end
        end
    }),
    awful.key({
        modifiers = { Modkey, "Shift" },
        keygroup = "numrow",
        description = "move focused client to tag",
        group = "tag",
        on_press = function(index)
            if client.focus then
                local tag = client.focus.screen.tags[index]
                if tag then client.focus:move_to_tag(tag) end
            end
        end
    }),
    awful.key({
        modifiers = { Modkey, "Control", "Shift" },
        keygroup = "numrow",
        description = "toggle focused client on tag",
        group = "tag",
        on_press = function(index)
            if client.focus then
                local tag = client.focus.screen.tags[index]
                if tag then client.focus:toggle_tag(tag) end
            end
        end
    })
})

-- Client keybindings
client.connect_signal("request::default_mousebindings", function()
    awful.mouse.append_client_mousebindings({
        awful.button({}, 1,
                     function(c) c:activate({ context = "mouse_click" }) end),
        awful.button({ Modkey }, 1, function(c)
            c:activate({ context = "mouse_click", action = "mouse_move" })
        end),
        awful.button({ Modkey }, 3, function(c)
            c:activate({ context = "mouse_click", action = "mouse_resize" })
        end)
    })
end)

client.connect_signal("request::default_keybindings", function()
    awful.keyboard.append_client_keybindings({
        awful.key({ Modkey }, "f", function(c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end, { description = "toggle fullscreen", group = "client" }),
        awful.key({ Modkey }, "y", function(c)
            c.floating = not c.floating
            c:raise()
        end, { description = "toggle floating", group = "client" }),
        awful.key({ Modkey }, "w", function(c) c:kill() end,
                  { description = "close", group = "client" }),
        awful.key({ Modkey, "Shift" }, "h",
                  function(c) c:move_to_screen(screen[1]) end,
                  { description = "move client to screen 2", group = "screen" }),
        awful.key({ Modkey, "Shift" }, "l",
                  function(c) c:move_to_screen(screen[2]) end,
                  { description = "move client to screen 2", group = "screen" })
    })
end)

-- Scratchpad keys
awful.keyboard.append_global_keybindings({
    awful.key({ Modkey }, "a",
              function() scratchpad.discord_scratch:toggle() end,
              { description = "discord scratchpad", group = "scratchpad" }),
    awful.key({ Modkey }, "s",
              function() scratchpad.spotify_scratch:toggle() end,
              { description = "spotify scratchpad", group = "scratchpad" }),
    awful.key({ Modkey }, "r",
              function() scratchpad.mailspring_scratch:toggle() end,
              { description = "mailspring scratchpad", group = "scratchpad" })
})
