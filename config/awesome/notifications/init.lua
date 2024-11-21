local naughty = require("naughty")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
local helpers = require("helpers")
local gears = require("gears")

-- Normal Notification
naughty.connect_signal("request::display", function(n)
	naughty.layout.box({
		notification = n,
		type = "notification",
		cursor = "hand2",
		maximum_width = dpi(500),
		maximum_height = dpi(250),
		shape = helpers.ui.rrect(beautiful.border_radius),
	})

	if helpers.misc.table_includes({ "lain_popup" }, n.category) then
		return
	end

	gears.timer({
		timeout = n.timeout,
		call_now = false,
		autostart = true,
		callback = function()
			n:destroy()
		end,
	})
end)

-- Error Handling
naughty.connect_signal("request::display_error", function(message, startup)
	naughty.notification({
		urgency = "critical",
		title = "Oops, an error happened" .. (startup and " during startup!" or "!"),
		message = message,
	})
end)
