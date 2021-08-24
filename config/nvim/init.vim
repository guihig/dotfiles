call plug#begin('~/.vim/plugged')

" Colorschemes
Plug 'sainnhe/sonokai'

Plug 'kyazdani42/nvim-web-devicons'
Plug 'dstein64/vim-startuptime'
Plug 'romgrk/barbar.nvim'
Plug 'glepnir/galaxyline.nvim'
Plug 'easymotion/vim-easymotion'
Plug 'mhinz/vim-startify'
Plug 'simeji/winresizer'
Plug 'kevinhwang91/rnvimr'
Plug 'psliwka/vim-smoothie'
Plug 'edkolev/tmuxline.vim'
Plug 'mg979/vim-visual-multi', { 'branch': 'master' }

" Telescope requirements
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'

" Code Utils
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-surround'
Plug 'JoosepAlviste/nvim-ts-context-commentstring'
Plug 'b3nj5m1n/kommentary'
Plug 'Raimondi/delimitMate'

Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  

" Code Config
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'mhartington/formatter.nvim'
Plug 'vim-test/vim-test'

call plug#end()

let mapleader = " "

nnoremap <F12> :source ~/.config/nvim/init.vim<CR>

lua require("nvim")
