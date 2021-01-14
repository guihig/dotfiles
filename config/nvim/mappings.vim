" --------------------------
" --- functions

" Terminal Function
let g:term_buf = 0
let g:term_win = 0
function! TermToggle(height)
    if win_gotoid(g:term_win)
        hide
    else
        try
            exec "botright sbuffer " . g:term_buf
            exec "resize " . a:height
        catch
            botright new
            exec "resize " . a:height

            call termopen($SHELL, {"detach": 0})
            let g:term_buf = bufnr("")
            set nonumber
            set norelativenumber
            set signcolumn=no
        endtry
        startinsert!
        let g:term_win = win_getid()
    endif
endfunction

"au TermOpen * startinsert
"au BufEnter,BufWinEnter,WinEnter term://* startinsert
"au BufLeave term://* stopinsert

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

" Toggle terminal on/off (neovim)
nnoremap <A-t> :call TermToggle(12)<CR>
inoremap <A-t> <Esc>:call TermToggle(12)<CR>
tnoremap <A-t> <C-\><C-n>:call TermToggle(12)<CR>

" --------------------------
" --- set no highlight
nnoremap <Leader><CR> :noh<cr>

" --------------------------
" --- exit terminal mode
tnoremap <C-o> <C-\><C-n>

" --------------------------
" --- save file
nnoremap <S-w> :update<CR>

" --------------------------
" --- split buffer verticaly
nnoremap <silent> <leader>v :vsplit<CR>
