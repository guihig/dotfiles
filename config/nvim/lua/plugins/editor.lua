local keymap = vim.keymap
return {
	{ "tpope/vim-repeat" },
	{ "psliwka/vim-smoothie" },
	{
		"m4xshen/hardtime.nvim",
		dependencies = { "MunifTanjim/nui.nvim", "nvim-lua/plenary.nvim" },
		opts = {
			max_count = 10,
			disable_mouse = false,
			allow_different_key = true,
		},
	},
	{
		"ggandor/leap.nvim",
		config = function()
			keymap.set({ "n", "x", "o" }, "<leader>s", "<Plug>(leap)")
			keymap.set({ "n", "x", "o" }, "<leader>S", "<Plug>(leap-from-window)")
		end,
	},
}
