local awful = require("awful")

local startup_cmds = { "wal -R", "picom", "nm-applet --no-agent" }

for app = 1, #startup_cmds do awful.spawn.once(startup_cmds[app]) end

awesome.restart()
