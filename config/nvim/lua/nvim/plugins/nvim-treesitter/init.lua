require'nvim-treesitter.configs'.setup {
    highlight = {enable = true},
    incremental_selection = {enable = true},
    indent = {enable = true},
    ensure_installed = {
        'haskell', 'lua', 'json', 'css', 'html', 'vue', 'typescript'
    }
}

vim.o.foldmethod = 'expr'
vim.o.foldexpr = 'nvim_treesitter#foldexpr()'
