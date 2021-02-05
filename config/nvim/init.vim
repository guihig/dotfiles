" Vim Plug
call plug#begin('~/.vim/plugged')

" Colorshemes
Plug 'sainnhe/sonokai'
Plug 'joshdick/onedark.vim'
Plug 'rakr/vim-one'
Plug 'sainnhe/gruvbox-material'
Plug 'gruvbox-community/gruvbox'
Plug 'ayu-theme/ayu-vim'

" Utils
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'edkolev/tmuxline.vim'
Plug 'ryanoasis/vim-devicons'
Plug 'mhinz/vim-startify'
Plug 'simeji/winresizer'
Plug 'ThePrimeagen/harpoon'
Plug 'easymotion/vim-easymotion'
Plug 'kevinhwang91/rnvimr'

" Telescope requirements
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'

" Git
Plug 'airblade/vim-gitgutter'

" Code utils
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-endwise'
Plug 'preservim/nerdcommenter'

" Snippets
Plug 'honza/vim-snippets'
Plug 'SirVer/ultisnips'

" Shl
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'elixir-editors/vim-elixir'
Plug 'posva/vim-vue'
Plug 'dag/vim-fish'

" Code Config
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'vim-test/vim-test'
Plug 'puremourning/vimspector'

call plug#end()

" Remapping leader key
let mapleader = " "

" Necessary sources
luafile $HOME/.config/nvim/lua/treesitter.lua


