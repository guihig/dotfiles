" --------------------------
" --- coc settings
" Coc Bindings
" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
" <cr> could be remapped by other vim plugin, try `:verbose imap <CR>`.
if exists('*complete_info')
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocActionAsync('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>F  <Plug>(coc-format-selected)
nmap <leader>F  <Plug>(coc-format-selected)
nmap <silent> <leader>f  :call CocAction('format')<CR>
nmap <silent> <leader>o  :call CocAction('runCommand', 'editor.action.organizeImport')<CR>

autocmd Filetype typescript,javascript,css,scss,sass setlocal tabstop=2

" Remap keys for applying codeAction to the current buffer.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings for CoCList
" Show all diagnostics.
"nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
"" Manage extensions.
"nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
"" Show commands.
"nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
"" Find symbol of current document.
"nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
"" Search workspace symbols.
"nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
"" Do default action for next item.
"nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
"" Do default action for previous item.
"nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
"" Resume latest coc list.
"nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>

" allow to scroll in the preview
set mouse=a

" mappings
nnoremap <silent> <leader><space> :<C-u>CocFzfList<CR>
nnoremap <silent> <leader>a       :<C-u>CocFzfList diagnostics<CR>
nnoremap <silent> <leader>b       :<C-u>CocFzfList diagnostics --current-buf<CR>
nnoremap <silent> <leader>c       :<C-u>CocFzfList commands<CR>
nnoremap <silent> <leader>e       :<C-u>CocFzfList extensions<CR>
nnoremap <silent> <leader>l       :<C-u>CocFzfList location<CR>
nnoremap <silent> <leader>o       :<C-u>CocFzfList outline<CR>
nnoremap <silent> <leader>s       :<C-u>CocFzfList symbols<CR>
nnoremap <silent> <leader>p       :<C-u>CocFzfListResume<CR>

" Coc Prettier
command! -nargs=0 Prettier :CocCommand prettier.formatFile

" Coc Explorer
" let g:coc_explorer_global_presets = {
" "\   '.vim': {
" "\     'root-uri': '~/.vim',
" "\   },
" "\   'tab': {
" "\     'position': 'tab',
" "\     'quit-on-open': v:true,
" "\   },
" "\   'floating': {
" "\     'position': 'floating',
" "\     'open-action-strategy': 'sourceWindow',
" "\   },
" "\   'floatingTop': {
" "\     'position': 'floating',
" "\     'floating-position': 'center-top',
" "\     'open-action-strategy': 'sourceWindow',
" "\   },
" "\   'floatingLeftside': {
" "\     'position': 'floating',
" "\     'floating-position': 'left-center',
" "\     'floating-width': 50,
" "\     'open-action-strategy': 'sourceWindow',
" "\   },
" "\   'floatingRightside': {
" "\     'position': 'floating',
" "\     'floating-position': 'right-center',
" "\     'floating-width': 50,
" "\     'open-action-strategy': 'sourceWindow',
" "\   },
" "\   'simplify': {
" "\     'file-child-template': '[selection | clip | 1] [indent][icon | 1] [filename omitCenter 1]'
" "\   }
" "\ }
" "
" nmap <C-n> :CocCommand explorer<CR>
