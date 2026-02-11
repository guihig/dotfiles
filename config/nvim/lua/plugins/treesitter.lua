return {
	"nvim-treesitter/nvim-treesitter",
	build = ":TSUpdate",
	config = function()
		require("nvim-treesitter.configs").setup({
			highlight = { enable = true },
			incremental_selection = { enable = true },
			indent = { enable = true },
			sync_install = false,
			auto_install = true,
			ignore_install = {},
			ensure_installed = {
				"haskell",
				"lua",
				"json",
				"css",
				"html",
				"vue",
				"http",
				"typescript",
				"qmljs",
				-- "latex",
				"fish",
				"vim",
				"elixir",
				"scss",
				"markdown",
				"markdown_inline",
				"regex",
				"bash",
				"vimdoc",
			},
		})

		require("ts_context_commentstring").setup({
			enable_autocmd = false,
		})

		vim.g.skip_ts_context_commentstring_module = true
		vim.opt.foldmethod = "expr"
		vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
	end,
}
