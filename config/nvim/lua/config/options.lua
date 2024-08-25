local HOME = vim.fn.expand("$HOME")

-- Variables
vim.g.mapleader = " "
vim.g.loaded_python_provider = 0
vim.g.loaded_ruby_provider = 0
vim.g.loaded_perl_provider = 0

-- Options
vim.opt.termguicolors = true
vim.o.mouse = "a"
vim.o.encoding = "UTF-8"
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
vim.o.undodir = HOME .. "/.vim/undodir"
vim.o.undofile = true
vim.o.incsearch = true
vim.o.showmode = false
vim.o.cot = "menuone,noselect"
vim.o.shortmess = vim.o.shortmess .. "c"
vim.o.cmdheight = 0
vim.o.updatetime = 100
vim.o.scrolloff = 8
vim.o.signcolumn = "yes:1"
vim.o.foldlevelstart = 20
vim.o.smoothscroll = true
