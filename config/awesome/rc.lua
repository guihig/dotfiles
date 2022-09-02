-- awesome_mode: api-level=4:screen=on
-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Consts
HOME_DIR = os.getenv("HOME")

-- Imports
local apps = require("apps")
local beautiful = require("beautiful")
local gears = require("gears")
require("awful.autofocus")

package.loaded["naughty.dbus"] = {}

-- Defaults
Terminal = apps.terminal
Modkey = "Mod4"

-- Theme
local theme_dir = gears.filesystem.get_configuration_dir() .. "theme/"
beautiful.init(theme_dir .. "theme.lua")

require("startup")

require("bindings")

require("layout")

require("rules")

require("notifications")

require("tags")

require("bar")

-- Window Focus
client.connect_signal("mouse::enter", function(c)
    c:activate({ context = "mouse_enter", raise = false })
end)
