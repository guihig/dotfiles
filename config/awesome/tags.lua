local awful = require("awful")

screen.connect_signal("request::desktop_decoration", function(s)
    -- Each screen has its own tag table.
    local tags = { "1", "2", "3", "4", "5" }
    for _, t in ipairs(tags) do
        awful.tag.add(t, {
            screen = s,
            layout = awful.layout.suit.tile,
            gap_single_client = true,
            gap = 3,
            selected = t == "1"
        })
    end
end)
