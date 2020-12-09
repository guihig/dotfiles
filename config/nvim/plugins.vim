" Vim Plug
call plug#begin('~/.vim/plugged')

" Colorshemes
Plug 'https://github.com/morhetz/gruvbox'
Plug 'https://github.com/ayu-theme/ayu-vim'
Plug 'https://github.com/nanotech/jellybeans.vim'

" Utils
Plug 'https://github.com/junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'https://github.com/junegunn/fzf.vim'
Plug 'https://github.com/vim-airline/vim-airline'
Plug 'https://github.com/vim-airline/vim-airline-themes'
"Plug 'https://github.com/preservim/nerdtree' |
"    \ Plug 'https://github.com/xuyuanp/nerdtree-git-plugin' |
Plug 'https://github.com/ryanoasis/vim-devicons'
Plug 'https://github.com/mhinz/vim-startify'
Plug 'https://github.com/simeji/winresizer'
Plug 'https://github.com/kevinhwang91/rnvimr'

" Git
Plug 'https://github.com/tpope/vim-fugitive'
Plug 'https://github.com/airblade/vim-gitgutter'

" Code utils
Plug 'https://github.com/tpope/vim-surround'
Plug 'https://github.com/preservim/nerdcommenter'
Plug 'https://github.com/tpope/vim-dotenv'
Plug 'https://github.com/907th/vim-auto-save'
Plug 'https://github.com/elixir-editors/vim-elixir'
Plug 'https://github.com/sheerun/vim-polyglot'
Plug 'https://github.com/easymotion/vim-easymotion'

" Code Config
Plug 'https://github.com/neoclide/coc.nvim', {'branch': 'release'}
Plug 'https://github.com/antoinemadec/coc-fzf'
Plug 'nvim-lua/completion-nvim'
Plug 'https://github.com/vim-test/vim-test'
Plug 'https://github.com/raimondi/delimitmate'

call plug#end()
