local gears = require("gears")

local M = {}

M.polybar = gears.filesystem.get_xdg_config_home() .. "polybar/launch.sh"
M.terminal = "alacritty"
M.discord = "discord"
M.flameshot = "flameshot gui"
M.mailspring = "mailspring"
M.spotify = "spotify"
M.launcher =
    "rofi -modi drun,run,ssh -show drun -matching fuzzy -show-icons -drun-icon-theme -lines 20 -padding 25 -width 30 -columns 1"
M.editor = os.getenv("EDITOR") or "vim"
M.notification_center = "kill -s USR1 $(pidof deadd-notification-center)"

return M
