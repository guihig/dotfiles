local get_hex = require("cokeline/hlgroups").get_hl_attr
local keymap = vim.keymap

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
        {
            text = function(buffer)
                return buffer.is_modified and "●" or ""
            end,
            fg = function(buffer)
                return buffer.is_modified and get_hex("Function", "fg") or nil
            end,
            delete_buffer_on_left_click = true,
            truncation = { priority = 1 }
        },
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

local opts = { noremap = true, silent = true }
keymap.set("n", "<S-A-h>", "<Plug>(cokeline-focus-prev)", opts)
keymap.set("n", "<S-A-l>", "<Plug>(cokeline-focus-next)", opts)
