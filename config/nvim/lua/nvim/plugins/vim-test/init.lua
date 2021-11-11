Keybind.g({
    {'n', 't<C-n>', ':TestNearest<CR>', {silent = true}},
    {'n', 't<C-f>', ':TestFile<CR>', {silent = true}},
    {'n', 't<C-s>', ':TestSuite<CR>', {silent = true}},
    {'n', 't<C-l>', ':TestLast<CR>', {silent = true}},
    {'n', 't<C-g>', ':TestVisit<CR>', {silent = true}}
})

vim.api.nvim_exec([[
  let g:nicemux_elixir_tmux_session = 'elixir-test'
  function! NiceMux(cmd) abort
    call system('tmux send-keys -t ' . g:nicemux_elixir_tmux_session . ':1.1 ENTER')

    call system('tmux send-keys -t ' . g:nicemux_elixir_tmux_session . ':1.1 "clear; ' . a:cmd . '" ENTER')
  endfunction

  let g:test#custom_strategies = {"nicemux": function("NiceMux")}
  let test#strategy = "nicemux"
]], true)

-- Variable.g({["test#strategy"] = "dispatch", ["test#preserve_screen"] = 1})
