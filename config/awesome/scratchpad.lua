local bling = require("modules.bling")
local awful = require("awful")
local apps = require("apps")

local M = {}

M.discord_scratch = bling.module.scratchpad({
	command = apps.discord,
	rule = { class = "discord" },
	sticky = false,
	autoclose = false,
	floating = true,
	geometry = { x = 0, y = 0 },
	dont_focus_before_close = true,
})

M.discord_scratch:connect_signal("turn_on", function(c)
	local s = awful.screen.focused()
	local width = s.geometry.width * 0.71
	local height = s.geometry.height * 0.71
	c.client:geometry({
		x = 0 + s.geometry.x,
		y = s.geometry.y + (s.geometry.height - height) / 2,
		width = width,
		height = height,
	})
end)

M.spotify_scratch = bling.module.scratchpad({
	command = apps.spotify,
	rule = { name = "Spotify" },
	sticky = false,
	autoclose = false,
	floating = true,
	geometry = { x = 0, y = 0 },
	dont_focus_before_close = true,
})

M.spotify_scratch:connect_signal("turn_on", function(c)
	local s = awful.screen.focused()
	local width = s.geometry.width * 0.71
	local height = s.geometry.height * 0.71
	c.client.floating = true
	c.client:geometry({
		x = s.geometry.x + (s.geometry.width - width) / 2,
		y = s.geometry.y + (s.geometry.height - height) / 2,
		width = width,
		height = height,
	})
end)

M.mailspring_scratch = bling.module.scratchpad({
	command = apps.mailspring,
	rule = { class = "Mailspring" },
	sticky = false,
	autoclose = false,
	floating = true,
	geometry = { x = 0, y = 0 },
	dont_focus_before_close = true,
})

M.mailspring_scratch:connect_signal("turn_on", function(c)
	local s = awful.screen.focused()
	local width = s.geometry.width * 0.71
	local height = s.geometry.height * 0.71
	c.client:geometry({
		x = s.geometry.x + (s.geometry.width - width) / 2,
		y = s.geometry.y + (s.geometry.height - height) / 2,
		width = width,
		height = height,
	})
end)

return M
