local keymap = vim.keymap
return {
	{ "echasnovski/mini.move", version = "*" },
	{ "echasnovski/mini.surround", version = "*" },
	{
		"echasnovski/mini.bufremove",
		version = "*",
		config = function()
			local keymap_opts = { silent = true, noremap = true }
			keymap.set("n", "<A-w>", ":bdelete", keymap_opts)
		end,
	},
}
