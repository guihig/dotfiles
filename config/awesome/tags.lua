local awful = require("awful")
local bling = require("modules.bling")

screen.connect_signal("request::desktop_decoration", function(s)
	local layout = awful.layout.suit.tile
	if s.index == 3 then
		layout = bling.layout.equalarea
	end
	for i = 1, 5, 1 do
		awful.tag.add(i, {
			screen = s,
			layout = layout,
			centered_layout_master_fill_policy = "master_width_factor",
			selected = i == 1 and true or false,
			gap_single_client = true,
			gap = 3,
		})
	end
end)
