require"nvim-treesitter.configs".setup {
    highlight = { enable = true },
    incremental_selection = { enable = true },
    indent = { enable = true },
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
        "scss",
        "markdown",
        "markdown_inline"
    }
}

vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
