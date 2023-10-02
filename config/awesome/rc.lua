-- awesome_mode: api-level=4:screen=on
-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Imports
local apps = require("apps")
-- local beautiful = require("beautiful")
-- local gears = require("gears")
--
-- package.loaded["naughty.dbus"] = {}

-- Defaults
Terminal = apps.terminal
Modkey = "Mod4"

-- Theme
-- local theme_dir = gears.filesystem.get_configuration_dir() .. "theme/"
-- beautiful.init(theme_dir .. "theme.lua")

require("bindings")

require("layout")

require("startup")

require("titlebars")

require("rules")

require("notifications")

require("tags")

require("bar")
