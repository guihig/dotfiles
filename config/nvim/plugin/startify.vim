" --------------------------
" --- startify settings
" 'Most Recent Files' number
let g:startify_files_number           = 1
" Update session automatically as you exit vim
let g:startify_session_persistence    = 1
" Simplify the startify list to just recent files and sessions
let g:startify_lists = [
  \ { 'type': 'sessions',  'header': ['   Saved sessions'] },
  \ { 'type': 'dir',       'header': ['   Recent files'] },
  \ ]
