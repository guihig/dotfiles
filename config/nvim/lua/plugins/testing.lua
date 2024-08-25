local keymap = vim.keymap

return {
	{
		"akinsho/toggleterm.nvim",
		opts = {

			open_mapping = [[<C-\>]],
			shade_terminals = false,
			direction = "float",
		},
		config = function(_, opts)
			require("toggleterm").setup(opts)
			local keymap_opts = { noremap = true, silent = true }
			keymap.set("t", "<C-N>", [[<C-\><C-N>]], keymap_opts)
		end,
	},
	{
		"vim-test/vim-test",
		config = function(_, _)
			vim.cmd([[
              let test#strategy = "toggleterm"
            ]])

			local opts = { silent = true }
			keymap.set("n", "t<C-n>", ":TestNearest<CR>", opts)
			keymap.set("n", "t<C-f>", ":TestFile<CR>", opts)
			keymap.set("n", "t<C-s>", ":TestSuite<CR>", opts)
			keymap.set("n", "t<C-l>", ":TestLast<CR>", opts)
			keymap.set("n", "t<C-g>", ":TestVisit<CR>", opts)
		end,
	},
}
