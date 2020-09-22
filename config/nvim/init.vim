syntax on

set timeoutlen=1000 ttimeoutlen=0
set guicursor=
set relativenumber
set nohlsearch
set hidden
set noerrorbells
set tabstop=4 softtabstop=4
set shiftwidth=4
set expandtab
set smartindent
set nu
set nowrap
set smartcase
set noswapfile
set nobackup
set undodir=~/.vim/undodir
set undofile
set incsearch
set noshowmode

" Give more space for displaying messages.
set cmdheight=2

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=50

call plug#begin('~/.vim/plugged')

Plug 'https://github.com/morhetz/gruvbox'
Plug 'https://github.com/junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'https://github.com/junegunn/fzf.vim'
Plug 'https://github.com/itchyny/lightline.vim'
Plug 'https://github.com/preservim/nerdtree'

call plug#end()

" GruvBox
set background=dark
colorscheme gruvbox

let mapleader = " "

" LightLine
set laststatus=2
let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ }

" NerdTREE
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

" Keybinds
set pastetoggle=<F3>
map <C-c> :Commands<CR>
map <C-n> :NERDTreeToggle<CR>
nnoremap <Leader>y "+y
nnoremap <Leader>+ :vertical resize +5<CR>
nnoremap <Leader>- :vertical resize -5<CR>
