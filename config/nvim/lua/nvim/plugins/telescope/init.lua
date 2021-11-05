local actions = require('telescope.actions')
local previewers = require('telescope.previewers')
local putils = require('telescope.previewers.utils')
local pfiletype = require('plenary.filetype')

Keybind.g({
    {'n', '<leader>p', '<cmd>Telescope find_files<CR>', {noremap = true}},
    {'n', '<leader>h', '<cmd>Telescope help_tags<CR>', {noremap = true}},
    {'n', '<C-f>', '<cmd>Telescope live_grep<CR>', {noremap = true}},
    {'n', '<C-b>', '<cmd>Telescope buffers<CR>', {noremap = true}}
})

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

require('telescope').setup {
    defaults = {
        --[[ file_previewer = require'telescope.previewers'.cat.new,
        grep_previewer = require'telescope.previewers'.cat.new,
        qflist_previewer = require'telescope.previewers'.cat.new, ]]
        buffer_previewer_maker = new_maker,
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
