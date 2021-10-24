Command.cmd({
    [[augroup highlight_yank
    autocmd!
    au TextYankPost * silent! lua vim.highlight.on_yank{higroup="StatusLine", timeout=300}
augroup END]]
})
