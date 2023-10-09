local keymap = vim.keymap

local opts = { silent = true, noremap = true }
keymap.set("n", "<A-w>", function()
	require("bufdelete").bufdelete(0, true)
end, opts)
