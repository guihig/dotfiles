local awful = require("awful")

local startup_cmds = { "picom", "nm-applet --no-agent" }

awful.spawn.raise_or_spawn("wal -R")
for app = 1, #startup_cmds do awful.spawn.once(startup_cmds[app]) end
