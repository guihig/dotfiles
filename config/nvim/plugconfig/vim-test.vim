" --------------------------
" --- vim-test settings
function! DispatchEnv(cmd) abort
    let env_file = getcwd() . "/.env"
    if filereadable(env_file)
        return "eval $(egrep -v '^\\#' " . env_file . " | xargs) " . a:cmd
    else
        return a:cmd
    endif
endfunction

let g:test#custom_transformations = {"dispatch": function("DispatchEnv")}
let g:test#transformation = 'dispatch'
let g:test#preserve_screen = 1

let test#strategy = "dispatch"

nmap <silent> t<C-n> :TestNearest<CR>
nmap <silent> t<C-f> :TestFile<CR>
nmap <silent> t<C-s> :TestSuite<CR>
nmap <silent> t<C-l> :TestLast<CR>
nmap <silent> t<C-g> :TestVisit<CR>
