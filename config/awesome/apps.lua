local M = {}

M.terminal = "kitty"
M.discord = "vesktop --disable-features=WebRtcAllowInputVolumeAdjustment"
-- M.discord = "discord --disable-features=WebRtcAllowInputVolumeAdjustment"
M.flameshot = "flameshot gui"
M.lockscreen = "slock"
M.spotify = "spotify"
M.launcher =
	"rofi -modi drun,run,ssh -show drun -matching fuzzy -show-icons -drun-icon-theme -lines 20 -padding 25 -width 30 -columns 1"
M.editor = os.getenv("EDITOR") or "vim"

return M
