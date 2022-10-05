local keymap = vim.keymap

local opts = {silent = true}
    keymap.set("n", "t<C-n>", ":TestNearest<CR>", opts)
    keymap.set("n", "t<C-f>", ":TestFile<CR>", opts)
    keymap.set("n", "t<C-s>", ":TestSuite<CR>", opts)
    keymap.set("n", "t<C-l>", ":TestLast<CR>", opts)
    keymap.set("n", "t<C-g>", ":TestVisit<CR>", opts)

-- local function HarpunxTest(cmd)
--     require("harpoon.tmux").sendCommand(9, string.format("clear; %s", cmd))
-- end

vim.api.nvim_exec([[
  let g:nicemux_test_session = 'test'
  function! NiceMux(cmd) abort
    call system('tmux send-keys -t ' . g:nicemux_test_session . ':1.1 ENTER')

    let root_dir = finddir('.git/..', expand('%:p:h').';')
    call system('tmux send-keys -t ' . g:nicemux_test_session . ':1.1 "cd ' . root_dir . '" ENTER')

    call system('tmux send-keys -t ' . g:nicemux_test_session . ':1.1 "clear; echo ' . a:cmd . '; ' . a:cmd . '" ENTER')
  endfunction

  let g:test#custom_strategies = {"nicemux": function("NiceMux")}
  let test#strategy = "nicemux"
]], true)

-- Variable.g({
--     ["test#custom_strategies"] = {harpunx = HarpunxTest},
--     ["test#strategy"] = "harpoon",
--     ["test#preserve_screen"] = 1,
--     ["test#elixir#exunit#options"] = "--trace"
-- })
