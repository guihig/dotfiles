require("nvim-treesitter.configs").setup({
	highlight = { enable = true },
	incremental_selection = { enable = true },
	indent = { enable = true },
	ensure_installed = {
		"haskell",
		"lua",
		"json",
		"css",
		"html",
		"vue",
		"http",
		"typescript",
		"latex",
		"fish",
		"vim",
		"elixir",
		"scss",
		"markdown",
		"markdown_inline",
	},
})

require("ts_context_commentstring").setup({
	enable_autocmd = false,
})

vim.g.skip_ts_context_commentstring_module = true
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
