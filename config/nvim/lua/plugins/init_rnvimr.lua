local keymap = vim.keymap

local opts = { noremap = true, silent = true }
keymap.set("t", "<A-i>", "<C-\\><C-n>:RnvimrResize<CR>", opts)
keymap.set("n", "<A-q>", ":RnvimrToggle<CR>", opts)
keymap.set("t", "<A-q>", "<C-\\><C-n>:RnvimrToggle<CR>", opts)

-- vim.g.loaded_netrw = 1
-- vim.g.loaded_netrwPlugin = 1
vim.g.rnvimr_enable_ex = 1
vim.g.rnvimr_enable_picker = 1
vim.g.rnvimr_enable_bw = 1
vim.g.rnvimr_hide_gitignore = 1
vim.g.rnvimr_ranger_cmd = { "ranger", "--cmd=set draw_borders both" }
vim.g.rnvimr_shadow_winblend = 100
vim.g.rnvimr_action = {
	["<C-t>"] = "NvimEdit tabedit",
	["<C-x>"] = "NvimEdit split",
	["<C-v>"] = "NvimEdit vsplit",
	["gw"] = "JumpNvimCwd",
	["yw"] = "EmitRangerCwd",
}

vim.cmd([[
  highlight link RnvimrNormal CursorLine
]])
