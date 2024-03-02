require("smart-splits").setup({
	ignored_buftypes = {
		"nofile",
		"quickfix",
		"prompt",
	},
	ignored_filetypes = { "NvimTree" },
	default_amount = 3,
	at_edge = "wrap",
	move_cursor_same_row = false,
	cursor_follows_swapped_bufs = true,
	resize_mode = {
		quit_key = "<ESC>",
		resize_keys = { "h", "j", "k", "l" },
		silent = false,
		hooks = {
			on_enter = nil,
			on_leave = nil,
		},
	},
	ignored_events = {
		"BufEnter",
		"WinEnter",
	},
	log_level = "info",
})

vim.keymap.set("n", "<C-e>", require("smart-splits").start_resize_mode)

vim.keymap.set("n", "<A-h>", require("smart-splits").resize_left)
vim.keymap.set("n", "<A-j>", require("smart-splits").resize_down)
vim.keymap.set("n", "<A-k>", require("smart-splits").resize_up)
vim.keymap.set("n", "<A-l>", require("smart-splits").resize_right)

vim.keymap.set("n", "<C-h>", require("smart-splits").move_cursor_left)
vim.keymap.set("n", "<C-j>", require("smart-splits").move_cursor_down)
vim.keymap.set("n", "<C-k>", require("smart-splits").move_cursor_up)
vim.keymap.set("n", "<C-l>", require("smart-splits").move_cursor_right)

vim.keymap.set("n", "<C-A-h>", require("smart-splits").swap_buf_left)
vim.keymap.set("n", "<C-A-j>", require("smart-splits").swap_buf_down)
vim.keymap.set("n", "<C-A-k>", require("smart-splits").swap_buf_up)
vim.keymap.set("n", "<C-A-l>", require("smart-splits").swap_buf_right)
