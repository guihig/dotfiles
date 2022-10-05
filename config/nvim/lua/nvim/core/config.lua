local USER = vim.fn.expand("$USER")

-- Cmds
vim.cmd([[
  autocmd FileType elixir setlocal ts=2 sts=2 sw=2
  autocmd FileType javascript setlocal ts=2 sts=2 sw=2
  autocmd FileType typescript setlocal ts=2 sts=2 sw=2
  autocmd FileType vue setlocal ts=2 sts=2 sw=2

  augroup highlight_yank
    autocmd!
    au TextYankPost * silent! lua vim.highlight.on_yank{higroup="StatusLine", timeout=300}
  augroup END

  highlight! link TSSymbol OrangeItalic
  highlight! link TSStringEscape Purple

  set nocompatible
  filetype plugin on
  syntax on
]])

-- Variables
vim.g.python3_host_prog = "/usr/bin/python"
vim.g.loaded_python_provider = 0
vim.g.loaded_ruby_provider = 0
vim.g.loaded_perl_provider = 0

-- Options
vim.o.mouse = "a"
vim.o.encoding = "UTF-8"
vim.o.pastetoggle = "<F3>"
vim.o.hidden = true
vim.o.timeoutlen = 1000
vim.o.ttimeoutlen = 0
vim.o.relativenumber = true
vim.o.errorbells = false
vim.o.tabstop = 4
vim.o.softtabstop = 2
vim.o.shiftwidth = 2
vim.o.expandtab = true
vim.o.smartindent = true
vim.o.nu = true
vim.o.wrap = false
vim.o.smartcase = true
vim.o.swapfile = false
vim.o.backup = false
vim.o.writebackup = false
vim.o.undodir = "/home/" .. USER .. "/.vim/undodir"
vim.o.undofile = true
vim.o.incsearch = true
vim.o.showmode = false
vim.o.cot = "menuone,noselect"
vim.o.shortmess = vim.o.shortmess .. "c"
vim.o.cmdheight = 2
vim.o.updatetime = 250
vim.o.scrolloff = 8
vim.o.termguicolors = true
vim.o.signcolumn = "yes:1"
vim.o.foldlevelstart = 20
