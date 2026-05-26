local keymap = vim.keymap

return {
	{ "simeji/winresizer" },
	{ "tpope/vim-repeat" },
	{ "psliwka/vim-smoothie" },
	-- {
	-- 	"m4xshen/hardtime.nvim",
	-- 	dependencies = { "MunifTanjim/nui.nvim", "nvim-lua/plenary.nvim" },
	-- 	opts = {
	-- 		max_count = 10,
	-- 		disable_mouse = false,
	-- 		allow_different_key = true,
	-- 	},
	-- },
	{
		"y3owk1n/undo-glow.nvim",
		version = "*",
	},
	{
		"smoka7/hop.nvim",
		version = "*",
		opts = {
			jump_on_sole_occurrence = false,
		},
		config = function(_, opts)
			require("hop").setup(opts)
			local hop = require("hop")
			local directions = require("hop.hint").HintDirection

			keymap.set({ "n", "x", "o", "v" }, "<leader>g", hop.hint_anywhere, { remap = true })
			keymap.set({ "n", "x", "o", "v" }, "<leader>s", hop.hint_patterns, { remap = true })
			keymap.set({ "n", "x", "o", "v" }, "<leader>w", hop.hint_words, { remap = true })

			keymap.set("", "f", function()
				hop.hint_char1({ direction = directions.AFTER_CURSOR, current_line_only = true })
			end, { remap = true })
			keymap.set("", "F", function()
				hop.hint_char1({ direction = directions.BEFORE_CURSOR, current_line_only = true })
			end, { remap = true })
			keymap.set("", "t", function()
				hop.hint_char1({ direction = directions.AFTER_CURSOR, current_line_only = true, hint_offset = -1 })
			end, { remap = true })
			keymap.set("", "T", function()
				hop.hint_char1({ direction = directions.BEFORE_CURSOR, current_line_only = true, hint_offset = 1 })
			end, { remap = true })
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
		"windwp/nvim-autopairs",
		event = "InsertEnter",
		opts = {
			disable_filetype = {
				"TelescopePrompt",
				"vim",
				"guihua",
				"guihua_rust",
				"clap_input",
			},
			disable_in_macro = true,
		},
		config = function(_, opts)
			local npairs = require("nvim-autopairs")
			npairs.setup(opts)
			npairs.add_rules(require("nvim-autopairs.rules.endwise-elixir"))
			npairs.add_rules(require("nvim-autopairs.rules.endwise-lua"))
			npairs.add_rules(require("nvim-autopairs.rules.endwise-ruby"))
		end,
	},
}
