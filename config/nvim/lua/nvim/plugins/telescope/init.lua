Keybind.g({
    {'n', '<C-p>', '<cmd>Telescope find_files<CR>', {noremap = true}},
    {'n', '<C-f>', '<cmd>Telescope live_grep<CR>', {noremap = true}},
    {'n', '<C-b>', '<cmd>Telescope buffers<CR>', {noremap = true}}
})

require('telescope').setup {
    defaults = {
        file_previewer = require'telescope.previewers'.cat.new,
        grep_previewer = require'telescope.previewers'.cat.new,
        qflist_previewer = require'telescope.previewers'.cat.new
    }
}
