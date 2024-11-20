local bar = require("ui.bar")
local notification_panel = require("ui.panels.notifications")

screen.connect_signal("request::desktop_decoration", function(s)
	bar(s)
	notification_panel(s)
end)
