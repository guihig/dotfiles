local keymap = vim.keymap

local actions = require("telescope.actions")
local previewers = require("telescope.previewers")
local putils = require("telescope.previewers.utils")
local pfiletype = require("plenary.filetype")

local keymap_opts = { noremap = true }
keymap.set("n", "<leader>p", "<cmd>Telescope find_files<CR>", keymap_opts)
keymap.set("n", "<leader>h", "<cmd>Telescope help_tags<CR>", keymap_opts)
keymap.set("n", "<C-f>", "<cmd>Telescope live_grep<CR>")
keymap.set("n", "<leader>b", "<cmd>Telescope buffers<CR>", keymap_opts)

local new_maker = function(filepath, bufnr, opts)
	opts = opts or {}
	if opts.use_ft_detect == nil then
		local ft = pfiletype.detect(filepath)
		-- Here for example you can say: if ft == "xyz" then this_regex_highlighing else nothing end
		if ft == "elixir" then
			opts.use_ft_detect = false
			putils.regex_highlighter(bufnr, ft)
		end
	end
	previewers.buffer_previewer_maker(filepath, bufnr, opts)
end

require("telescope").setup({
	defaults = {
		buffer_previewer_maker = new_maker,
		vimgrep_arguments = {
			"rg",
			"--hidden",
			"--color=never",
			"--no-heading",
			"--with-filename",
			"--line-number",
			"--column",
			"--smart-case",
		},
		file_ignore_patterns = {
			"node_modules",
			".git",
			".idea",
			".elixis_ls",
			"_build/",
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
})

require("telescope").load_extension("vimwiki")
require("telescope").load_extension("fzy_native")

vim.cmd([[
  function! MaybeTelescope()
    if argc() == 1 && isdirectory(argv()[0])
          execute "Telescope find_files"
      endif
  endfunction

  autocmd VimEnter * :call MaybeTelescope()
]])
