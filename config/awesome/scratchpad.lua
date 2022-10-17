local bling = require("modules.bling")
local apps = require("apps")

local M = {}

M.discord_scratch = bling.module.scratchpad {
    command = apps.discord,
    rule = { class = "discord" },
    sticky = true,
    autoclose = false,
    floating = true,
    geometry = { x = 0, y = 140, height = 800, width = 1000 },
    reapply = true,
    dont_focus_before_close = true
}

M.spotify_scratch = bling.module.scratchpad {
    command = apps.spotify,
    rule = { name = "Spotify" },
    sticky = true,
    autoclose = false,
    floating = true,
    geometry = { x = 277, y = 156, height = 768, width = 1366 },
    reapply = true,
    dont_focus_before_close = true
}

M.mailspring_scratch = bling.module.scratchpad {
    command = apps.mailspring,
    rule = { class = "Mailspring" },
    sticky = true,
    autoclose = false,
    floating = true,
    geometry = { x = 360, y = 140, height = 800, width = 1200 },
    reapply = true,
    dont_focus_before_close = true
}

return M
