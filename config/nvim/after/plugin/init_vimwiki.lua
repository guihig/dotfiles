local keymap = vim.keymap

local keymap_opts = { noremap = true }

keymap.set("n", "<leader>ww", "<cmd>VimwikiIndex <CR>", keymap_opts)
