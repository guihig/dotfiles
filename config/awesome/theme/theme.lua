-- #-------- Imports --------#
local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local rnotification = require("ruled.notification")
local gfs = require("gears.filesystem")
local gears = require("gears")
local awful = require("awful")

local dpi = xresources.apply_dpi

local themes_path = gfs.get_themes_dir()

local theme = {}
local xtheme = xresources.get_current_theme()

-- #-------- Fonts --------#
theme.font = "JetBrainsMono Nerd Font "
theme.font_name = theme.font .. "Medium 10"
theme.icon_font = "Material Design Icons "

-- #-------- Colors --------#
-- From 0 to 15  xresources colors
for i = 0, 15, 1 do
	theme["color" .. i] = xtheme["color" .. i]
end

theme.bg = xtheme.background
theme.fg = xtheme.foreground

theme.fg_normal = theme.fg
theme.bg_normal = theme.bg
theme.fg_focus = theme.fg
theme.bg_focus = theme.color8
theme.fg_urgent = theme.color15
theme.bg_urgent = theme.color1

theme.white = "#FFFFFF"
theme.black = "#000000"
theme.transparent = "#00000000"

--- Accent colors
function theme.random_accent_color()
	local accents = {
		theme.color9,
		theme.color10,
		theme.color11,
		theme.color12,
		theme.color13,
		theme.color14,
	}

	local i = math.random(1, #accents)
	return accents[i]
end

-- #-------- Icons --------#
theme.icons = {
	volume = "󰕾",
	volume_low = "󰖀",
	volume_off = "󰖁",
	volume_mute = "󰝟",
	cpu = "󰻠",
	calendar = "󰃮",
	mem = "󰍛",
	fs = "󰋊",
	float = "󰉈",
	tile = "󱇙",
	bell = "󰂜",
}
-- #-------- Common --------#
theme.wallpaper = themes_path .. "default/background.png"
theme.icon_theme = "Papirus-Dark"
theme.useless_gap = dpi(4)
theme.border_radius = dpi(8)

-- #-------- Titlebar --------#
theme.titlebar_fg_normal = theme.fg_normal
theme.titlebar_bg_normal = theme.bg_normal

theme.titlebar_fg_focus = theme.fg_focus
theme.titlebar_bg_focus = theme.bg_focus

theme.titlebar_fg_urgent = theme.color15
theme.titlebar_bg_urgent = theme.color1

-- #-------- Systray --------#
theme.bg_systray = theme.transparent
theme.systray_icon_spacing = dpi(4)
theme.systray_max_rows = 1

-- #-------- Wibar --------#
-- theme.wibar_opacity = 0.85
theme.wibar_fg = theme.fg
theme.wibar_bg = theme.bg
theme.wibar_height = dpi(40)

-- #-------- Taglist --------#
theme.taglist_bg_focus = theme.color2
theme.taglist_fg_focus = theme.fg
theme.taglist_bg_urgent = theme.red
theme.taglist_fg_urgent = theme.fg
theme.taglist_bg_occupied = theme.color3 .. "33"
theme.taglist_fg_occupied = theme.fg
theme.taglist_bg_empty = theme.color3 .. "33"
theme.taglist_fg_empty = theme.fg
theme.taglist_disable_icon = true

-- #-------- Tasklist --------#
theme.tasklist_shape = gears.shape.rounded_rect
theme.tasklist_shape_border_width = dpi(1)
theme.tasklist_shape_border_color = theme.color0

-- #-------- Notifications --------#
rnotification.connect_signal("request::rules", function()
	rnotification.append_rule({
		rule = { urgency = "critical" },
		properties = { bg = theme.bg_urgent, fg = theme.foreground },
	})
	rnotification.append_rule({
		rule = {},
		properties = {
			icon_size = dpi(80),
			screen = awful.screen.focused or awful.screen.preferred,
		},
	})
end)

theme.notification_font = theme.font
theme.notification_bg = theme.bg
theme.notification_fg = theme.color15
theme.notification_border_width = dpi(1)
theme.notification_border_color = theme.color0
theme.notification_shape = gears.shape.rounded_rect
theme.notification_opacity = 0.8
theme.notification_margin = dpi(4)
theme.notification_spacing = dpi(4)

theme.notification_panel_bg = theme.color0
theme.notification_panel_bg_alt = theme.color8 .. "66"

-- #-------- Clients --------#
theme.border_color_active = theme.color8
theme.border_color_normal = theme.color0
theme.border_width = dpi(1)

-- #-------- Layout Icons --------#
theme.layout_fairh = themes_path .. "default/layouts/fairhw.png"
theme.layout_fairv = themes_path .. "default/layouts/fairvw.png"
theme.layout_floating = themes_path .. "default/layouts/floatingw.png"
theme.layout_magnifier = themes_path .. "default/layouts/magnifierw.png"
theme.layout_max = themes_path .. "default/layouts/maxw.png"
theme.layout_fullscreen = themes_path .. "default/layouts/fullscreenw.png"
theme.layout_tilebottom = themes_path .. "default/layouts/tilebottomw.png"
theme.layout_tileleft = themes_path .. "default/layouts/tileleftw.png"
theme.layout_tile = themes_path .. "default/layouts/tilew.png"
theme.layout_tiletop = themes_path .. "default/layouts/tiletopw.png"
theme.layout_spiral = themes_path .. "default/layouts/spiralw.png"
theme.layout_dwindle = themes_path .. "default/layouts/dwindlew.png"
theme.layout_cornernw = themes_path .. "default/layouts/cornernww.png"
theme.layout_cornerne = themes_path .. "default/layouts/cornernew.png"
theme.layout_cornersw = themes_path .. "default/layouts/cornersww.png"
theme.layout_cornerse = themes_path .. "default/layouts/cornersew.png"

return theme
