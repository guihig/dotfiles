require'nvim-treesitter.configs'.setup {
    highlight = {enable = true},
    incremental_selection = {enable = true},
    indent = {enable = true},
    ensure_installed = {
        'haskell', 'lua', 'json', 'css', 'html', 'vue', 'typescript'
    }
}
