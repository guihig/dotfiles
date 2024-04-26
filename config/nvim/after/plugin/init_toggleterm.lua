require("toggleterm").setup({
	open_mapping = [[<C-\>]],
	shade_terminals = false,
	direction = "float",
})

local keymap = vim.keymap
local opts = { noremap = true, silent = true }
keymap.set("t", "<C-N>", [[<C-\><C-N>]], opts)
