local utils = require("utils")
local keymap = vim.keymap

local opts = { noremap = true }
local silent_opts = { silent = true, noremap = true }

-- Copy to Clipboard
keymap.set("n", "<leader>y", "\"+y", opts)
keymap.set("v", "<leader>y", "\"+y", opts)
keymap.set("n", "<leader>Y", "gg\"+yG", opts)

keymap.set("n", "<F12>", utils.reload_nvim, opts)

-- Exit Term Mode
keymap.set("t", "<C-o>", "<C-\\><C-n>", opts)

-- Visual Select to Search
keymap.set("v", "//", "y/\\V<C-R>=escape(@\",'/\\')<CR><CR>", opts)

-- Save buffer
keymap.set("n", "W", ":update<CR>", opts)

-- Split buffer
keymap.set("n", "<leader>v", ":vsplit<CR>", silent_opts)
keymap.set("n", "<leader>x", ":split<CR>", silent_opts)

-- Must Have Remaps
keymap.set("n", "Y", "yg_", opts)

keymap.set("n", "n", "nzzzv", opts)
keymap.set("n", "N", "Nzzzv", opts)

keymap.set("v", "<", "<gv", opts)
keymap.set("v", ">", ">gv", opts)

keymap.set("n", "J", "mzJ`z", opts)

keymap.set("i", ",", ",<c-g>u", opts)
keymap.set("i", ".", ".<c-g>u", opts)
keymap.set("i", ":", ":<c-g>u", opts)
keymap.set("i", ";", ";<c-g>u", opts)
keymap.set("i", "!", "!<c-g>u", opts)
keymap.set("i", "?", "?<c-g>u", opts)

-- keymap.set("n", "p", "\"0p", opts)
-- keymap.set("v", "p", "\"0p", opts)
-- keymap.set("x", "p", "\"0p", opts)

-- Move Lines
keymap.set("n", "<A-j>", ":m .+1<CR>==", opts)
keymap.set("n", "<A-k>", ":m .-2<CR>==", opts)
keymap.set("i", "<A-j>", "<Esc>:m .+1<CR>==gi", opts)
keymap.set("i", "<A-k>", "<Esc>:m .-2<CR>==gi", opts)
keymap.set("v", "<A-j>", ":m '>+1<CR>gv==gv", opts)
keymap.set("v", "<A-k>", ":m '<-2<CR>gv==gv", opts)
