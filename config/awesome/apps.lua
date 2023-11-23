local M = {}

M.terminal = "alacritty"
M.discord =
	"discord --ignore-gpu-blocklist --disable-features=UseOzonePlatform --enable-features=VaapiVideoDecoder --use-gl=desktop --enable-gpu-rasterization --enable-zero-copy"
M.flameshot = "flameshot gui"
M.telegram = "telegram-desktop"
M.whatsapp = "whatsapp-for-linux"
M.lockscreen = "slock"
M.spotify = "spotify"
M.launcher =
	"rofi -modi drun,run,ssh -show drun -matching fuzzy -show-icons -drun-icon-theme -lines 20 -padding 25 -width 30 -columns 1"
M.editor = os.getenv("EDITOR") or "vim"

return M
