local lsp_location = require("lsp_location")

return {
	{
		"folke/lazydev.nvim",
		ft = "lua",
		cmd = "LazyDev",
		opts = {
			library = {
				lsp_location["awesomewm_lib"],
				"~/.config/awesome/modules/rubato",
				"~/.config/awesome/modules/bling",
				"~/.config/awesome/modules/color",
				"~/.config/awesome/lain",
				{ path = "luvit-meta/library", words = { "vim%.uv" } },
				{ path = "LazyVim", words = { "LazyVim" } },
				{ path = "lazy.nvim", words = { "LazyVim" } },
			},
		},
	},
	{
		"mistricky/codesnap.nvim",
		build = "make",
		keys = {
			{ "<leader>cc", "<cmd>CodeSnap<cr>", mode = "x", desc = "Save selected code snapshot into clipboard" },
			{ "<leader>cs", "<cmd>CodeSnapSave<cr>", mode = "x", desc = "Save selected code snapshot in ~/Pictures" },
		},
		opts = {
			save_path = "~/Pictures",
			has_breadcrumbs = true,
			has_line_number = true,
			bg_padding = 0,
		},
	},
	{
		"saghen/blink.cmp",
		dependencies = {
			{ "rafamadriz/friendly-snippets" },
		},
		version = "1.*",
		opts = {
			enabled = function()
				return not vim.tbl_contains({
					"clap_input",
					"guihua",
					"guihia_rust",
					"sagadiagnostic",
					"sagafinder",
					"sagarename",
					"diff",
					"saga_unitest",
				}, vim.bo.filetype) and vim.bo.buftype ~= "prompt" and vim.b.completion ~= false
			end,
			keymap = {
				preset = "none",
				["<C-space>"] = { "show", "show_documentation", "hide_documentation" },
				["<C-e>"] = { "hide" },
				["<CR>"] = { "select_and_accept", "fallback" },

				["<Up>"] = { "select_prev", "fallback" },
				["<Down>"] = { "select_next", "fallback" },
				["<C-p>"] = { "select_prev", "fallback_to_mappings" },
				["<C-n>"] = { "select_next", "fallback_to_mappings" },

				["<C-b>"] = { "scroll_documentation_up", "fallback" },
				["<C-f>"] = { "scroll_documentation_down", "fallback" },

				["<C-K>"] = {
					function(cmp)
						cmp.show({ providers = { "snippets" } })
					end,
				},

				["<Tab>"] = { "snippet_forward", "fallback" },
				["<S-Tab>"] = { "snippet_backward", "fallback" },

				["<C-k>"] = { "show_signature", "hide_signature", "fallback" },
			},
			signature = { enabled = true },
			appearance = {
				nerd_font_variant = "mono",
			},
			completion = { documentation = { auto_show = true } },
			snippets = { preset = "default" },
			sources = {
				default = { "lsp", "path", "snippets", "buffer" },
			},
			fuzzy = { implementation = "prefer_rust_with_warning" },
		},
		opts_extend = { "sources.default" },
	},
}
