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
		"ggandor/leap.nvim",
		config = function()
			require("leap").opts.preview_filter = function(ch0, ch1, ch2)
				return not (ch1:match("%s") or ch0:match("%a") and ch1:match("%a") and ch2:match("%a"))
			end
			require("leap").opts.equivalence_classes = { " \t\r\n", "([{", ")]}", "'\"`" }

			vim.api.nvim_set_hl(0, "LeapBackdrop", { link = "Comment" })
			vim.api.nvim_set_hl(0, "LeapMatch", {
				fg = "white",
				bold = true,
				nocombine = true,
			})

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
