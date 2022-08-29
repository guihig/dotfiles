require'nvim-treesitter.configs'.setup {
    highlight = {enable = true},
    incremental_selection = {enable = true},
    indent = {enable = true},
    context_commentstring = {enable = true, enable_autocmd = false},
    ensure_installed = {
        'haskell', 'lua', 'json', 'css', 'html', 'vue', 'typescript', 'latex',
        'fish', 'elixir'
    },
    rainbow = {
        enable = true,
        extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
        max_file_lines = nil -- Do not enable for files with more than n lines, int
    }
}

vim.o.foldmethod = 'expr'
vim.o.foldexpr = 'nvim_treesitter#foldexpr()'
