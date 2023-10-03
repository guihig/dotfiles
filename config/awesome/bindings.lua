local awful = require("awful")
local utils = require("utils")
local apps = require("apps")
local scratchpad = require("scratchpad")
local naughty = require("naughty")

local keys = { mod = "Mod4", ctrl = "Control", shift = "Shift", alt = "Mod1" }

local directions = {
	{ k = "h", d = "left" },
	{ k = "l", d = "right" },
	{ k = "k", d = "up" },
	{ k = "j", d = "down" },
}

-- General keys
awful.keyboard.append_global_keybindings({
	awful.key({ keys.mod }, "q", awesome.restart, { description = "reload awesome", group = "awesome" }),
	awful.key({ keys.mod, keys.shift }, "q", awesome.quit, { description = "quit awesome", group = "awesome" }),
	awful.key({ keys.mod }, "Return", function()
		awful.spawn(apps.terminal)
	end, { description = "open a terminal", group = "launcher" }),
	awful.key({ keys.mod }, "space", function()
		awful.spawn(apps.launcher)
	end, { description = "open a launcher", group = "launcher" }),
	awful.key({ keys.mod, keys.shift }, "=", function()
		awful.spawn.with_shell(apps.notification_center)
	end, {
		description = "open the notification center",
		group = "launcher",
	}),
	awful.key({ keys.mod }, "c", function()
		naughty.destroy_all_notifications()
	end, { description = "clear notifications", group = "util" }),
	awful.key({}, "Print", function()
		awful.spawn(apps.flameshot)
	end, { description = "open the flameshot gui", group = "launcher" }),
	-- awful.key {
	--     modifiers = {},
	--     key = "XF86AudioRaiseVolume",
	--     group = "media",
	--     description = "increase volume",
	--     on_press = function()
	--         audio_daemon:get_default_sink():volume_up(5)
	--     end
	-- },
	-- awful.key {
	--     modifiers = {},
	--     key = "XF86AudioLowerVolume",
	--     group = "media",
	--     description = "decrease volume",
	--     on_press = function()
	--         audio_daemon:get_default_sink():volume_down(5)
	--     end
	-- },
	-- awful.key {
	--     modifiers = {},
	--     key = "XF86AudioMute",
	--     group = "media",
	--     description = "mute volume",
	--     on_press = function()
	--         audio_daemon:get_default_sink():toggle_mute()
	--     end
	-- }
})

-- Focus keybindings
local focus_keybindings = {
	awful.key({ keys.mod }, ";", function()
		awful.screen.focus_relative(1)
	end, { description = "focus the other screen", group = "screen" }),
}

for _, value in ipairs(directions) do
	table.insert(
		focus_keybindings,
		awful.key({ keys.mod }, value.k, function()
			awful.client.focus.global_bydirection(value.d, client.focus, true)
		end, { description = "focus " .. value.d .. " client", group = "client" })
	)
end

awful.keyboard.append_global_keybindings(focus_keybindings)

-- Swap keybindings
local swap_keybindings = {}

for _, value in ipairs(directions) do
	table.insert(
		swap_keybindings,
		awful.key({ keys.mod, keys.ctrl }, value.k, function()
			awful.client.swap.bydirection(value.d)
		end, { description = "swap " .. value.d .. " client", group = "client" })
	)
end

awful.keyboard.append_global_keybindings(swap_keybindings)

-- Layout related keybindings
awful.keyboard.append_global_keybindings({
	awful.key(
		{ keys.mod },
		"u",
		awful.client.urgent.jumpto,
		{ description = "jump to urgent client", group = "client" }
	),
	awful.key({ keys.mod, keys.ctrl }, "[", function()
		awful.tag.incnmaster(-1)
	end, { description = "decrease master", group = "layout" }),
	awful.key({ keys.mod, keys.ctrl }, "]", function()
		awful.tag.incnmaster(1)
	end, { description = "increase master", group = "layout" }),
	awful.key({ keys.mod }, "]", function()
		awful.tag.incmwfact(0.05)
	end, { description = "increase master width factor", group = "layout" }),
	awful.key({ keys.mod }, "[", function()
		awful.tag.incmwfact(-0.05)
	end, { description = "decrease master width factor", group = "layout" }),
	awful.key({ keys.mod, keys.shift }, "[", function()
		awful.client.incwfact(-0.05, client.focus)
	end, { description = "decrease client width factor", group = "layout" }),
	awful.key({ keys.mod, keys.shift }, "]", function()
		awful.client.incwfact(0.05, client.focus)
	end, { description = "increase client width factor", group = "layout" }),
	awful.key({ keys.mod }, "Tab", function()
		awful.layout.inc(1)
		utils.handle_master_count()
	end, { description = "select next", group = "layout" }),
	awful.key({ keys.mod, keys.shift }, "Tab", function()
		awful.layout.inc(-1)
		utils.handle_master_count()
	end, { description = "select previous", group = "layout" }),
})

-- @DOC_NUMBER_KEYBINDINGS@
awful.keyboard.append_global_keybindings({
	awful.key({
		modifiers = { keys.mod },
		keygroup = "numrow",
		description = "only view tag",
		group = "tag",
		on_press = function(index)
			local screen = awful.screen.focused()
			local tag = screen.tags[index]
			if tag then
				tag:view_only()
			end
		end,
	}),
	awful.key({
		modifiers = { keys.mod, keys.ctrl },
		keygroup = "numrow",
		description = "toggle tag",
		group = "tag",
		on_press = function(index)
			local screen = awful.screen.focused()
			local tag = screen.tags[index]
			if tag then
				awful.tag.viewtoggle(tag)
			end
		end,
	}),
	awful.key({
		modifiers = { keys.mod, keys.shift },
		keygroup = "numrow",
		description = "move focused client to tag",
		group = "tag",
		on_press = function(index)
			if client.focus then
				local tag = client.focus.screen.tags[index]
				if tag then
					client.focus:move_to_tag(tag)
				end
			end
		end,
	}),
	awful.key({
		modifiers = { keys.mod, keys.ctrl, keys.shift },
		keygroup = "numrow",
		description = "toggle focused client on tag",
		group = "tag",
		on_press = function(index)
			if client.focus then
				local tag = client.focus.screen.tags[index]
				if tag then
					client.focus:toggle_tag(tag)
				end
			end
		end,
	}),
})

-- Client keybindings
client.connect_signal("request::default_keybindings", function()
	awful.keyboard.append_client_keybindings({
		awful.key({ keys.mod }, "f", function(c)
			c.fullscreen = not c.fullscreen
			c:raise()
		end, { description = "toggle fullscreen", group = "client" }),
		awful.key({ keys.mod }, "y", function(c)
			c.floating = not c.floating
			c:raise()
		end, { description = "toggle floating", group = "client" }),
		awful.key({ keys.mod, keys.shift }, "r", function(c)
			c.floating = false
			c.maximized = false
			c.maximized_horizontal = false
			c.maximized_vertical = false
			c.sticky = false
			c.fullscreen = false
			c:raise()
		end, { description = "reset client props", group = "client" }),
		awful.key({ keys.mod, keys.shift }, "s", function(c)
			c.sticky = true
			c.ontop = true
			c:raise()
		end, { description = "sticky client", group = "client" }),
		awful.key({ keys.mod }, "w", function(c)
			c:kill()
		end, { description = "close", group = "client" }),
		awful.key({ keys.mod, keys.shift }, "h", function(c)
			local idx = c.screen.index
			c:move_to_screen(screen[idx - 1])
		end, {
			description = "move client to the previous screen",
			group = "screen",
		}),
		awful.key({ keys.mod, keys.shift }, "l", function(c)
			local idx = c.screen.index
			c:move_to_screen(screen[idx + 1])
		end, { description = "move client to the next screen", group = "screen" }),
		awful.key({
			modifiers = { keys.mod, keys.shift },
			key = "n",
			group = "client",
			description = "restore minimized",
			on_press = function()
				local c = awful.client.restore()
				if c then
					c:activate({ raise = true, context = "key.unminimize" })
				end
			end,
		}),
	})
end)

client.connect_signal("request::default_mousebindings", function()
	awful.mouse.append_client_mousebindings({ -- Focus a client
		awful.button({
			modifiers = {},
			button = 1,
			on_press = function(c)
				if c.can_focus ~= false then
					c:activate({ context = "mouse_click" })
				end
			end,
		}), -- Make a client floating and move it
		awful.button({
			modifiers = { keys.mod },
			button = 1,
			on_press = function(c)
				if c.can_move ~= false then
					c.floating = true
					c.maximized = false
					c.fullscreen = false
					c:activate({ context = "mouse_click", action = "mouse_move" })
				end
			end,
		}), -- Make a client floating and resize it
		awful.button({
			modifiers = { keys.mod },
			button = 3,
			on_press = function(c)
				if c.can_resize ~= false then
					c.floating = true
					c.maximized = false
					c.fullscreen = false
					c:activate({ context = "mouse_click", action = "mouse_resize" })
				end
			end,
		}),
	})
end)

-- Scratchpad keys
awful.keyboard.append_global_keybindings({
	awful.key({ keys.mod }, "a", function()
		scratchpad.discord_scratch:toggle()
	end, { description = "discord scratchpad", group = "scratchpad" }),
	awful.key({ keys.mod }, "s", function()
		scratchpad.spotify_scratch:toggle()
	end, { description = "spotify scratchpad", group = "scratchpad" }),
})
