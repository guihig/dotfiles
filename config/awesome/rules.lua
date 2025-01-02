local awful = require("awful")
local ruled = require("ruled")
local helpers = require("helpers")

ruled.client.connect_signal("request::rules", function()
	-- Global
	ruled.client.append_rule({
		rule = {},
		properties = {
			focus = awful.client.focus.filter,
			raise = true,
			screen = awful.screen.preferred,
			size_hints_honor = false,
			honor_workarea = true,
			honor_padding = true,
			placement = awful.placement.no_overlap + awful.placement.no_offscreen,
		},
	})

	-- Tasklist order
	ruled.client.append_rule({
		id = "tasklist_order",
		rule = {},
		properties = {},
		callback = awful.client.setslave,
	})

	-- Floating clients.
	ruled.client.append_rule({
		id = "floating",
		rule_any = {
			type = { "dialog" },
			instance = { "spad", "discord" },
			class = {
				"Arandr",
				"Spotify",
				"spotify",
				"vesktop",
				"telegram-desktop",
				"Slack",
				"Rocket.Chat",
				"gnome-calendar",
				"tsmapplication.exe",
			},
			name = {
				"Steam - Self Updater",
				"Android Emulator*",
				"JetBrains Toolbox",
				"Discord Updater",
				"splash",
				"Telegram",
				"TSM Login*",
				"TradeSkillMaster Application*",
			},
			role = { "pop-up" },
		},
		properties = { floating = true, placement = helpers.ui.placement.centered },
	})

	ruled.client.append_rule({
		rule_any = { class = { "steam_app_0", "steam", "steamwebhelper" } },
		except_any = {
			name = {
				"TSM Login*",
			},
		},
		properties = {
			tag = screen[1].tags[5],
			switch_to_tags = false,
			opacity = 1,
		},
	})
end)
