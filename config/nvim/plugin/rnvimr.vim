tnoremap <silent> <A-i> <C-\><C-n>:RnvimrResize<CR>
nnoremap <silent> <A-q> :RnvimrToggle<CR>
tnoremap <silent> <A-q> <C-\><C-n>:RnvimrToggle<CR>

let g:rnvimr_enable_ex = 1
let g:rnvimr_enable_picker = 1
let g:rnvimr_hide_gitignore = 1
let g:rnvimr_border_attr = {'fg': 14, 'bg': -1}
let g:rnvimr_enable_bw = 1
let g:rnvimr_shadow_winblend = 70
let g:rnvimr_ranger_cmd = 'ranger --cmd="set draw_borders both"'

highlight link RnvimrNormal CursorLine

let g:rnvimr_action = {
            \ '<C-t>': 'NvimEdit tabedit',
            \ '<C-x>': 'NvimEdit split',
            \ '<C-v>': 'NvimEdit vsplit',
            \ 'gw': 'JumpNvimCwd',
            \ 'yw': 'EmitRangerCwd'
            \ }

let g:rnvimr_presets = [
            \ {'width': 0.600, 'height': 0.600},
            \ {},
            \ {'width': 0.800, 'height': 0.800},
            \ {'width': 0.950, 'height': 0.950},
            \ ]
