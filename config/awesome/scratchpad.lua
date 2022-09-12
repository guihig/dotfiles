local bling = require("bling")
local apps = require("apps")

local M = {}

M.discord_scratch = bling.module.scratchpad {
    command = apps.discord, -- How to spawn the scratchpad
    rule = { class = "discord" }, -- The rule that the scratchpad will be searched by
    sticky = true, -- Whether the scratchpad should be sticky
    autoclose = false, -- Whether it should hide itself when losing focus
    floating = true, -- Whether it should be floating (MUST BE TRUE FOR ANIMATIONS)
    geometry = { x = 0, y = 140, height = 800, width = 1000 }, -- The geometry in a floating state
    reapply = true, -- Whether all those properties should be reapplied on every new opening of the scratchpad (MUST BE TRUE FOR ANIMATIONS)
    dont_focus_before_close = false -- When set to true, the scratchpad will be closed by the toggle function regardless of whether its focused or not. When set to false, the toggle function will first bring the scratchpad into focus and only close it on a second call
}

M.spotify_scratch = bling.module.scratchpad {
    command = apps.spotify,
    rule = { name = "Spotify" },
    sticky = true,
    autoclose = false,
    floating = true,
    geometry = { x = 277, y = 156, height = 768, width = 1366 },
    reapply = true,
    dont_focus_before_close = false
}

M.mailspring_scratch = bling.module.scratchpad {
    command = apps.mailspring, -- How to spawn the scratchpad
    rule = { class = "Mailspring" }, -- The rule that the scratchpad will be searched by
    sticky = true, -- Whether the scratchpad should be sticky
    autoclose = false, -- Whether it should hide itself when losing focus
    floating = true, -- Whether it should be floating (MUST BE TRUE FOR ANIMATIONS)
    geometry = { x = 360, y = 140, height = 800, width = 1200 }, -- The geometry in a floating state
    reapply = true, -- Whether all those properties should be reapplied on every new opening of the scratchpad (MUST BE TRUE FOR ANIMATIONS)
    dont_focus_before_close = false -- When set to true, the scratchpad will be closed by the toggle function regardless of whether its focused or not. When set to false, the toggle function will first bring the scratchpad into focus and only close it on a second call
}

return M
