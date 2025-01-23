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
			vim.api.nvim_set_hl(0, "LeapBackdrop", { link = "Comment" })
			keymap.set({ "n", "x", "o", "v" }, "<leader>s", "<Plug>(leap)")
			keymap.set({ "n", "x", "o", "v" }, "<leader>S", "<Plug>(leap-from-window)")
			-- Hide the (real) cursor when leaping, and restore it afterwards.
			vim.api.nvim_create_autocmd("User", {
				pattern = "LeapEnter",
				callback = function()
					vim.cmd.hi("Cursor", "blend=100")
					vim.opt.guicursor:append({ "a:Cursor/lCursor" })
				end,
			})
			vim.api.nvim_create_autocmd("User", {
				pattern = "LeapLeave",
				callback = function()
					vim.cmd.hi("Cursor", "blend=0")
					vim.opt.guicursor:remove({ "a:Cursor/lCursor" })
				end,
			})
		end,
	},
	{
		"ggandor/flit.nvim",
		config = function()
			require("flit").setup()
		end,
	},
	{
		"chrisgrieser/nvim-early-retirement",
		opts = {
			notificationOnAutoClose = true,
			deleteBufferWhenFileDeleted = true,
		},
		event = "VeryLazy",
	},
	{
		"theKnightsOfRohan/csvlens.nvim",
		dependencies = {
			"akinsho/toggleterm.nvim",
		},
		config = true,
	},
	{
		"NvChad/nvim-colorizer.lua",
		opts = {
			user_default_options = {
				mode = "virtualtext",
			},
		},
	},
	{
		"lewis6991/satellite.nvim",
		opts = {},
	},
}
