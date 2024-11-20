local naughty = require("naughty")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
local utils = require("utils")

-- Normal Notification
naughty.connect_signal("request::display", function(n)
	naughty.layout.box({
		notification = n,
		type = "notification",
		cursor = "hand2",
		maximum_width = dpi(500),
		maximum_height = dpi(250),
		shape = utils.ui.rrect(beautiful.border_radius),
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
