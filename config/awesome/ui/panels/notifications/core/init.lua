local wibox = require("wibox")

return function(s)
	s.notification_layout = require("ui.panels.notifications.core.inbox").layout

	return wibox.widget({
		s.notification_layout,
		layout = wibox.layout.fixed.vertical,
	})
end
