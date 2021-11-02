Variable.g({nvim_ipy_perform_mappings = 0, ipy_celldef = '^##'})
Keybind.g({
    -- Format code
    {'n', '<leader>jqt', '<cmd>RunQtConsole<CR>', {silent = true}},
    {'n', '<leader>jk', ':ConnectToPipenvKernel<CR>', {silent = true}},
    {'n', '<leader>jc', '<Plug>(IPy-RunCell)<CR>', {silent = true}},
    {'n', '<leader>ja', '<Plug>(IPy-RunAll)<CR>', {silent = true}},
    {'n', '<leader>ji', '<Plug>(IPy-WordObjInfo)<CR>', {silent = true}},
    {'n', '<leader>jr', '<Plug>(IPy-Run)<CR>', {silent = true}},
    {'v', '<leader>jr', '<Plug>(IPy-Run)<CR>', {silent = true}}
})
