" Remapping leader key
let mapleader = " "

" Vim Plug
call plug#begin('~/.vim/plugged')

" Colorshemes
Plug 'sainnhe/sonokai'
Plug 'tjdevries/colorbuddy.vim'
Plug 'tjdevries/gruvbuddy.nvim'

" Utils
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'edkolev/tmuxline.vim'
" Plug 'ryanoasis/vim-devicons'
Plug 'kyazdani42/nvim-web-devicons'
Plug 'mhinz/vim-startify'
Plug 'simeji/winresizer'
Plug 'ThePrimeagen/harpoon'
Plug 'easymotion/vim-easymotion'
Plug 'kevinhwang91/rnvimr'
Plug 'psliwka/vim-smoothie'
Plug 'mg979/vim-visual-multi', {'branch': 'master'}

" Telescope requirements
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'

" Git
Plug 'airblade/vim-gitgutter'

" Code utils
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-surround'
Plug 'preservim/nerdcommenter'
Plug 'jiangmiao/auto-pairs'
" Plug 'Raimondi/delimitMate'

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

" Sources
luafile $HOME/.config/nvim/lua/init.lua


