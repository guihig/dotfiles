local awful = require("awful")
local wibox = require("wibox")
local wrapper = require("widgets.wrapper")
local alsabar = require("handlers.audio")

local volume = {}

local function init()
	alsabar.bar:buttons(awful.util.table.join(
		awful.button({}, 1, function() -- left click
			awful.spawn(string.format("%s -e alsamixer", "kitty"))
		end),
		awful.button({}, 2, function() -- middle click
			os.execute(string.format("%s set %s 85%%", alsabar.cmd, alsabar.channel))
			alsabar.update()
		end),
		awful.button({}, 3, function() -- right click
			os.execute(string.format("%s set %s toggle", alsabar.cmd, alsabar.togglechannel or alsabar.channel))
			alsabar.update()
		end),
		awful.button({}, 4, function() -- scroll up
			os.execute(string.format("%s set %s 5%%+", alsabar.cmd, alsabar.channel))
			alsabar.update()
		end),
		awful.button({}, 5, function() -- scroll down
			os.execute(string.format("%s set %s 5%%-", alsabar.cmd, alsabar.channel))
			alsabar.update()
		end)
	))

	local widget = wibox.widget({
		layout = wibox.layout.fixed.horizontal,
		alsabar.icon,
		alsabar.bar,
	})
	return wrapper(widget)
end

return setmetatable(volume, {
	__call = function(_, ...)
		return init(...)
	end,
})
