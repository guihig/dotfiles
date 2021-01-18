" --------------------------
" --- buffer navigation
nnoremap <silent> <S-A-l> :bn<CR>
nnoremap <silent> <S-A-h> :bp<CR>
nnoremap <silent> <A-w> :bp\|bdelete #<CR>

" --------------------------
" --- source vim file
nnoremap <F12> :source ~/.config/nvim/init.vim<CR>

" --------------------------
" --- copy to clipboard
nnoremap <Leader>y "+y
vnoremap <Leader>y "+y
nnoremap <Leader>Y gg"+yG

" --------------------------
" --- move line
nnoremap <A-j> :m .+1<CR>==
nnoremap <A-k> :m .-2<CR>==
inoremap <A-j> <Esc>:m .+1<CR>==gi
inoremap <A-k> <Esc>:m .-2<CR>==gi
vnoremap <A-j> :m '>+1<CR>gv=gv
vnoremap <A-k> :m '<-2<CR>gv=gv

" --------------------------
" --- set no highlight
nnoremap <Leader><CR> :noh<cr>

" --------------------------
" --- exit terminal mode
tnoremap <C-o> <C-\><C-n>

" --------------------------
" --- visual selection to search
vnoremap // y/\V<C-R>=escape(@",'/\')<CR><CR>

" --------------------------
" --- save file
nnoremap <S-w> :update<CR>

" --------------------------
" --- split buffer verticaly
nnoremap <silent> <leader>v :vsplit<CR>
