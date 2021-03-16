Keybind.g({
    {
        't', '<A-i>', '<C-\\><C-n>:RnvimrResize<CR>',
        {noremap = true, silent = true}
    }, {'n', '<A-q>', ':RnvimrToggle<CR>', {noremap = true, silent = true}},
    {
        't', '<A-q>', '<C-\\><C-n>:RnvimrToggle<CR>',
        {noremap = true, silent = true}
    }
})

Variable.g({
    loaded_netrwPlugin = 1,
    rnvimr_enable_ex = 0,
    rnvimr_enable_picker = 1,
    rnvimr_hide_gitignore = 1,
    rnvimr_border_attr = {fg = 14, bg = -1},
    rnvimr_enable_bw = 1,
    rnvimr_shadow_winblend = 70,
    rnvimr_ranger_cmd = 'ranger --cmd="set draw_borders both"',
    rnvimr_action = {
        ['<C-t>'] = 'NvimEdit tabedit',
        ['<C-x>'] = 'NvimEdit split',
        ['<C-v>'] = 'NvimEdit vsplit',
        ['gw'] = 'JumpNvimCwd',
        ['yw'] = 'EmitRangerCwd'
    },
    rnvimr_presets = {
        {['width'] = 0.600, ['height'] = 0.600},
        {['width'] = 0.800, ['height'] = 0.800},
        {['width'] = 0.950, ['height'] = 0.950}
    }
})

Command.cmd({'highlight link RnvimrNormal CursorLine'})
