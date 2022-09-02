local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")

local M = { placement = {} }

-- Client helpers
M.placement.centered = function(c)
    return gears.timer.delayed_call(function()
        awful.placement.centered(c, {
            honor_padding = true,
            honor_workarea = true
        })
    end)
end

-- UI Helpers

M.spacer = function()
    return
        wibox.widget({ widget = wibox.container.margin, left = 5, right = 5 })
end

return M
