" Vim Plug
call plug#begin('~/.vim/plugged')

" Colorshemes
Plug 'https://github.com/sainnhe/sonokai'
Plug 'https://github.com/joshdick/onedark.vim'
Plug 'https://github.com/rakr/vim-one'
Plug 'sainnhe/gruvbox-material'
Plug 'gruvbox-community/gruvbox'
Plug 'https://github.com/nanotech/jellybeans.vim'
Plug 'https://github.com/ayu-theme/ayu-vim'

" Utils
Plug 'https://github.com/vim-airline/vim-airline'
Plug 'https://github.com/vim-airline/vim-airline-themes'
Plug 'https://github.com/ryanoasis/vim-devicons'
Plug 'https://github.com/mhinz/vim-startify'
Plug 'https://github.com/simeji/winresizer'
Plug 'https://github.com/ThePrimeagen/harpoon'
Plug 'https://github.com/easymotion/vim-easymotion'
Plug 'edkolev/tmuxline.vim'
Plug 'kevinhwang91/rnvimr'

" Telescope requirements
Plug 'https://github.com/nvim-lua/popup.nvim'
Plug 'https://github.com/nvim-lua/plenary.nvim'
Plug 'https://github.com/nvim-telescope/telescope.nvim'

" Git
Plug 'https://github.com/airblade/vim-gitgutter'

" Code utils
Plug 'https://github.com/tpope/vim-dispatch'
Plug 'https://github.com/tpope/vim-surround'
Plug 'https://github.com/preservim/nerdcommenter'
Plug 'https://github.com/raimondi/delimitmate'
Plug 'https://github.com/honza/vim-snippets'
Plug 'https://github.com/SirVer/ultisnips'

" Shl
Plug 'elixir-editors/vim-elixir'
Plug 'posva/vim-vue'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'https://github.com/dag/vim-fish'

" Code Config
Plug 'https://github.com/neoclide/coc.nvim', {'branch': 'release'}
" Plug 'prabirshrestha/vim-lsp'
" Plug 'mattn/vim-lsp-settings'
" Plug 'neovim/nvim-lspconfig'
" Plug 'nvim-lua/completion-nvim'
Plug 'https://github.com/vim-test/vim-test'
Plug 'puremourning/vimspector'

call plug#end()

" Remapping leader key
let mapleader = " "

" Necessary sources
luafile $HOME/.config/nvim/lua/treesitter.lua

let g:loaded_python_provider = 0
let g:loaded_perl_provider = 0
let loaded_netrwPlugin = 1
