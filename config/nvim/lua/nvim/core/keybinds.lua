local opts = {noremap = true}
local silent_opts = {silent = true, noremap = true}

Keybind.g({
    -- Format code
    {'n', '<leader>f', '<cmd>FormatWrite<CR>', opts},

    -- Buffer Nav
    {'n', '<S-A-h>', '<cmd>BufferPrevious<CR>', silent_opts},
    {'n', '<S-A-l>', '<cmd>BufferNext<CR>', silent_opts},
    {'n', '<A-w>', '<cmd>BufferClose<CR>', silent_opts},

    -- Copy to Clipboard
    {'n', '<leader>y', '"+y', opts},
    {'v', '<leader>y', '"+y', opts},
    {'n', '<leader>Y', 'gg"+yG', opts},

    {'n', '<F11>', ':luafile %<CR>', opts},
    {'n', '<F12>', '<cmd>lua Editor.reload_nvim()<CR>', opts},

    -- Set NoHL
    {'n', '<leader><cr>', ':noh<CR>', opts},

    -- Exit Term Mode
    {'t', '<C-o>', '<C-\\><C-n>', opts},

    -- Visual Select to Search
    {'v', '//', 'y/\\V<C-R>=escape(@",\'/\\\')<CR><CR>', opts},

    -- Save buffer
    {'n', 'W', ':update<CR>', opts},

    -- Split buffer
    {'n', '<leader>v', ':vsplit<CR>', silent_opts},
    {'n', '<leader>x', ':split<CR>', silent_opts},

    -- Must Have Remaps
    {'n', 'Y', 'yg_', opts},

    {'n', 'n', 'nzzzv', opts},
    {'n', 'N', 'Nzzzv', opts},

    {'v', '<', '<gv', opts},
    {'v', '>', '>gv', opts},

    {'n', 'J', 'mzJ`z', opts},

    {'i', ',', ',<c-g>u', opts},
    {'i', '.', '.<c-g>u', opts},
    {'i', ':', ':<c-g>u', opts},
    {'i', ';', ';<c-g>u', opts},
    {'i', '!', '!<c-g>u', opts},
    {'i', '?', '?<c-g>u', opts},

    -- Paste last Copy
    {'n', '<leader>p', '"_dP', opts},
    {'v', '<leader>p', '"_dP', opts},

    -- Move Lines
    {'n', '<A-j>', ':m .+1<CR>==', opts},
    {'n', '<A-k>', ':m .-2<CR>==', opts},
    {'i', '<A-j>', '<Esc>:m .+1<CR>==gi', opts},
    {'i', '<A-k>', '<Esc>:m .-2<CR>==gi', opts},
    {'v', '<A-j>', ':m \'>+1<CR>gv==gv', opts},
    {'v', '<A-k>', ':m \'<-2<CR>gv==gv', opts},
})
