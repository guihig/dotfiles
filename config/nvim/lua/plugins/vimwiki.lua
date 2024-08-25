local keymap = vim.keymap
return {
	"vimwiki/vimwiki",
	config = function()
		local keymap_opts = { noremap = true }
		keymap.set("n", "<leader>ww", "<cmd>VimwikiIndex <CR>", keymap_opts)
	end,
}
