local get_hex = require("cokeline/utils").get_hex

vim.api.nvim_set_hl(0, "TabLineFill", { bg = "none" })

local hl_group = "DiffChange"

require("cokeline").setup({
    default_hl = {
        fg = function(buffer)
            return buffer.is_focused and get_hex("Normal", "fg")
                       or get_hex("Comment", "fg")
        end,
        bg = function(buffer)
            return buffer.is_focused and get_hex(hl_group, "bg")
                       or get_hex("ColorColumn", "bg")
        end
    },

    components = {
        { text = " ", bg = "none" },
        {
            text = "",
            fg = function(buffer)
                return buffer.is_focused and get_hex(hl_group, "bg")
                           or get_hex("ColorColumn", "bg")
            end,
            bg = get_hex("Normal", "bg")
        },
        {
            text = function(buffer) return buffer.devicon.icon end,
            fg = function(buffer) return buffer.devicon.color end
        },
        { text = " " },
        {
            text = function(buffer) return buffer.filename .. "  " end,
            style = function(buffer)
                return buffer.is_focused and "bold" or nil
            end
        },
        { text = "", delete_buffer_on_left_click = true },
        {
            text = "",
            fg = function(buffer)
                return buffer.is_focused and get_hex(hl_group, "bg")
                           or get_hex("ColorColumn", "bg")
            end,
            bg = get_hex("Normal", "bg")
        }
    }
})
