local actions = require('telescope.actions')

Keybind.g({
    {'n', '<leader>p', '<cmd>Telescope find_files<CR>', {noremap = true}},
    {'n', '<leader>h', '<cmd>Telescope help_tags<CR>', {noremap = true}},
    {'n', '<C-f>', '<cmd>Telescope live_grep<CR>', {noremap = true}},
    {'n', '<C-b>', '<cmd>Telescope buffers<CR>', {noremap = true}}
})

require('telescope').setup {
    defaults = {
        --[[ file_previewer = require'telescope.previewers'.cat.new,
        grep_previewer = require'telescope.previewers'.cat.new,
        qflist_previewer = require'telescope.previewers'.cat.new, ]]
        vimgrep_arguments = {
            'rg', '--hidden', '--color=never', '--no-heading',
            '--with-filename', '--line-number', '--column', '--smart-case'
        },
        file_ignore_patterns = {
            "node_modules", ".git", ".idea", ".elixis_ls", "_build/"
        },
        mappings = {
            i = {
                --[[ ["<S-Tab>"] = actions.toggle_selection +
                    actions.move_selection_worse,
                ["<Tab>"] = actions.toggle_selection +
                    actions.move_selection_better, ]]
                ["<C-j>"] = actions.move_selection_next,
                ["<C-k>"] = actions.move_selection_previous, -- otherwise, just set the mapping to the function that you want it to be.
                ["<C-q>"] = actions.send_selected_to_qflist +
                    actions.open_qflist
            },
            n = {
                --[[ ["<S-Tab>"] = actions.toggle_selection +
                    actions.move_selection_worse,
                ["<Tab>"] = actions.toggle_selection +
                    actions.move_selection_better, ]]
                ["<C-q>"] = actions.send_selected_to_qflist +
                    actions.open_qflist
            }
        }
    }
}

vim.api.nvim_exec([[
  function! MaybeTelescope()
    if argc() == 1 && isdirectory(argv()[0])
          " Uncomment this to remove the Netrw buffer (optional)
          execute "bdelete"
          execute "Telescope find_files"
      endif
  endfunction

  autocmd VimEnter * :call MaybeTelescope()
]], true)
