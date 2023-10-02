local awful = require("awful")
local bling = require("modules.bling")

tag.connect_signal("request::default_layouts", function()
    awful.layout.append_default_layouts({
        awful.layout.suit.tile.left, awful.layout.suit.tile.right,
        awful.layout.suit.max, bling.layout.equalarea
        -- machi.default_layout
    })
end)

