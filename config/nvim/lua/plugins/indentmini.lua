return {
	"nvimdev/indentmini.nvim",
	-- event = "LazyFile",
	opts = {
		char = "│",
		exclude = {
			"help",
			"alpha",
			"dashboard",
			"neo-tree",
			"Trouble",
			"trouble",
			"lazy",
			"mason",
			"notify",
			"toggleterm",
			"lazyterm",
			"markdown",
		},
	},
	config = function(_, opts)
		require("indentmini").setup(opts)
		vim.api.nvim_set_hl(0, "IndentLine", { fg = "#302f3d" })
		vim.api.nvim_set_hl(0, "IndentLineCurrent", { fg = "#4a6141" })
	end,
}
