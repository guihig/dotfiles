local awful = require("awful")
local apps = require("apps")

local startup_cmds = { apps.discord, apps.spotify }

for app = 1, #startup_cmds do
	awful.spawn.with_shell(startup_cmds[app])
end
