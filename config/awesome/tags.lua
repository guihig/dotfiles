local awful = require("awful")
local bling = require("modules.bling")

screen.connect_signal("request::desktop_decoration", function(s)
    -- Each screen has its own tag table.
    local tags = { "1", "2", "3", "4", "5" }
    local layout = awful.layout.suit.tile
    if s.index == 3 then layout = bling.layout.equalarea end
    for _, t in ipairs(tags) do
        awful.tag.add(t, {
            screen = s,
            layout = layout,
            gap_single_client = true,
            gap = 3,
            selected = t == "1"
        })
    end
end)
