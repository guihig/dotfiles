local opts = {noremap = true}
local silent_opts = {silent = true, noremap = true}

Keybind.g({
    -- Buffer Nav
    {'n', '<S-A-l>', ':bn<CR>', silent_opts},
    {'n', '<S-A-h>', ':bp<CR>', silent_opts},
    {'n', '<A-w>', ':bp\\|bdelete #<CR>', opts},

    -- Source NVIM Config
    {'n', '<F12>', '<cmd>lua Editor.reload_nvim()<CR>', opts},

    -- Copy to Clipboard
    {'n', '<leader>y', '"+y', opts},
    {'v', '<leader>y', '"+y', opts},
    {'n', '<leader>Y', 'gg"+yG', opts},

    -- Set NoHL
    {'n', '<leader><CR>', ':noh<CR>', opts},

    -- Exit Term Mode
    {'t', '<C-o>', '<C-\\><C-n>', opts},

    -- Visual Select to Search
    {'v', '//', 'y/\\V<C-R>=escape(@",\'/\\\')<CR><CR>', opts},

    -- Save buffer
    {'n', 'W', ':update<CR>', opts},

    -- Split buffer Vertically
    {'n', '<leader>v', ':vsplit<CR>', silent_opts},

    -- Paste last Copy
    {'n', '<leader>p', '"_dP', opts},
    {'v', '<leader>p', '"_dP', opts},

    -- Move Lines
    -- TODO
    {'n', '<A-j>', ':m .+1<CR>==', opts},
    {'n', '<A-k>', ':m .-2<CR>==', opts},
    {'i', '<A-j>', '<Esc>:m .+1<CR>==gi', opts},
    {'i', '<A-k>', '<Esc>:m .-2<CR>==gi', opts},
    {'v', '<A-j>', ':m \'>+1<CR>gv==gv', opts},
    {'v', '<A-k>', ':m \'<-2<CR>gv==gv', opts},

})