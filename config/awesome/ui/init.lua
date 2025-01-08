local bar = require("ui.bar")

screen.connect_signal("request::desktop_decoration", function(s)
	bar(s)
end)
