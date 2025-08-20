local keymap = vim.keymap
return {
	{
		"echasnovski/mini.move",
		version = "*",
		config = function()
			require("mini.move").setup()
		end,
	},
	{
		"echasnovski/mini.surround",
		version = "*",
		config = function()
			require("mini.surround").setup()
		end,
	},
	{
		"echasnovski/mini.cursorword",
		version = "*",
		config = function()
			require("mini.cursorword").setup()
		end,
	},
	{
		"echasnovski/mini.bufremove",
		version = "*",
		options = {
			silent = false,
		},
		config = function(_, opts)
			require("mini.bufremove").setup(opts)
			local bufremove = require("mini.bufremove")
			local keymap_opts = { silent = true, noremap = true }
			keymap.set("n", "<A-w>", bufremove.delete, keymap_opts)
		end,
	},
}
