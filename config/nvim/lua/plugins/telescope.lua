local keymap = vim.keymap

return {
	"nvim-telescope/telescope.nvim",
	dependencies = {
		{ "nvim-lua/plenary.nvim" },
		{ "ElPiloto/telescope-vimwiki.nvim" },
		{ "nvim-telescope/telescope-fzy-native.nvim" },
	},
	opts = function()
		local actions = require("telescope.actions")
		local rg_ignore_patterns =
			"!{**/.git/*,**/node_modules/*,**/.idea/*,**/.elixir_ls/*,**/_build/*,**/yarn.lock,**/.yarn/*}"

		return {
			defaults = {
				buffer_previewer_maker = new_maker,
				vimgrep_arguments = {
					"rg",
					"--color=never",
					"--no-heading",
					"--with-filename",
					"--line-number",
					"--column",
					"--smart-case",
					"--trim",
					"--hidden",
					"--glob",
					rg_ignore_patterns,
				},
				mappings = {
					i = {
						["<C-j>"] = actions.move_selection_next,
						["<C-k>"] = actions.move_selection_previous,
						["<C-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
					},
					n = {
						["<C-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
					},
				},
			},
			pickers = {
				find_files = {
					find_command = {
						"rg",
						"--files",
						"--hidden",
						"--glob",
						rg_ignore_patterns,
					},
				},
			},
		}
	end,
	config = function(_, opts)
		require("telescope").setup(opts)

		local keymap_opts = { noremap = true }
		keymap.set("n", "<leader>p", "<cmd>Telescope find_files<CR>", keymap_opts)
		keymap.set("n", "<leader>h", "<cmd>Telescope help_tags<CR>", keymap_opts)
		keymap.set("n", "<C-f>", "<cmd>Telescope live_grep<CR>")
		keymap.set("n", "<leader>b", "<cmd>Telescope buffers<CR>", keymap_opts)

		require("telescope").load_extension("vimwiki")
		require("telescope").load_extension("fzy_native")
	end,
}
