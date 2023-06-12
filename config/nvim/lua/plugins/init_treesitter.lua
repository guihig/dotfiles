require"nvim-treesitter.configs".setup {
    highlight = { enable = true },
    incremental_selection = { enable = true },
    indent = { enable = false },
    context_commentstring = { enable = true, enable_autocmd = false },
    ensure_installed = {
        "haskell",
        "lua",
        "json",
        "css",
        "html",
        "vue",
        "typescript",
        "latex",
        "fish",
        "vim",
        "elixir",
        "scss"
    }
}

vim.o.foldmethod = "expr"
vim.o.foldexpr = "nvim_treesitter#foldexpr()"
