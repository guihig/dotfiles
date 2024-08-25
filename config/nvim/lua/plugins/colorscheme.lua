return {
	{
		"xiyaowong/transparent.nvim",
		opts = function()
			return {}
		end,
		config = function(_, opts)
			require("transparent").setup(opts)
		end,
	},
	{
		"sontungexpt/witch",
		priority = 1000,
		lazy = false,
		opts = function()
			return {}
		end,
		config = function(_, opts)
			require("witch").setup(opts)
		end,
	},

	-- Good ol Gruvbox Baby
	-- {
	-- 	"sainnhe/gruvbox-material",
	-- 	priority = 1000,
	-- 	lazy = false,
	-- 	config = function()
	-- 		vim.opt.background = "dark"
	-- 		vim.g.gruvbox_material_background = "hard"
	-- 		vim.g.gruvbox_material_enable_italic = 1
	-- 		vim.g.gruvbox_material_disable_italic_comment = 1
	-- 		vim.g.gruvbox_material_better_performance = 1
	-- 		vim.g.gruvbox_material_transparent_background = 1
	-- 		vim.g.gruvbox_material_enable_bold = 1
	-- 		vim.g.cs = "gruvbox-material"
	-- 		vim.cmd("colorscheme " .. vim.g.cs)
	-- 	end,
	-- },
}
