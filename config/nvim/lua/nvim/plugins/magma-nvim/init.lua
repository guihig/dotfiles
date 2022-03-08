Variable.g({
    magma_automatically_open_output = false,
    magma_show_mimetype_debug = true,
    magma_image_provider = "ueberzug"
})
Keybind.g({
    -- Format code
    {'n', '<leader>jqt', '<cmd>RunQtConsole<CR>', {silent = true}},
    {'n', '<leader>jk', ':MagmaInit<CR>', {silent = false}},
    {'n', '<leader>jc', ':MagmaDeinit<CR>', {silent = false}},
    {'n', '<leader>js', ':MagmaInterrupt<CR>', {silent = false}},
    {'n', '<leader>jl', ':MagmaEvaluateLine<CR>', {silent = false}},
    {'n', '<leader>jr', ':MagmaReevaluateCell<CR>', {silent = false}},
    {'x', '<leader>jr', ':<C-u>MagmaEvaluateVisual<CR>', {silent = false}},
    {
        'n', '<leader>ja', '<ESC>ggvG:<C-u>MagmaEvaluateVisual<CR>',
        {silent = false}
    }
})
