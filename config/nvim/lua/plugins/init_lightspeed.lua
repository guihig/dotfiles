local keymap = vim.keymap

require("lightspeed").setup({})

local opts = { noremap = true, silent = true }
keymap.set("n", "<leader>s", "<Plug>Lightspeed_s", opts)
keymap.set("n", "<leader>S", "<Plug>Lightspeed_S", opts)
keymap.set("v", "<leader>s", "<Plug>Lightspeed_s", opts)
keymap.set("v", "<leader>S", "<Plug>Lightspeed_S", opts)

vim.cmd("silent! unmap s")
vim.cmd("silent! unmap S")
