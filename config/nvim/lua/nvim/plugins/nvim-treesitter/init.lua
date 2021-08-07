require'nvim-treesitter.configs'.setup {
    highlight = {enable = true},
    incremental_selection = {enable = true},
    indent = {enable = true},
    context_commentstring = {enable = true},
    ensure_installed = {
        'haskell', 'lua', 'json', 'css', 'html', 'vue', 'typescript', 'latex'
    }
}

vim.o.foldmethod = 'expr'
vim.o.foldexpr = 'nvim_treesitter#foldexpr()'
