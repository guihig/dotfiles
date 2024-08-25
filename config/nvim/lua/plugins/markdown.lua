return {
	{
		"iamcco/markdown-preview.nvim",
		build = function()
			vim.fn["mkdp#util#install"]()
		end,
		config = function()
			vim.g.mkdp_filetypes = { "markdown", "vimwiki" }
		end,
	},
}
