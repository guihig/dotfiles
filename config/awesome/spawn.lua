local awful = require("awful")

local startup_cmds = { "vesktop", "spotify" }

for app = 1, #startup_cmds do
	awful.spawn.with_shell(startup_cmds[app])
end
