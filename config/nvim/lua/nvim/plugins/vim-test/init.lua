local dap = require("dap")

function _G.run_dap()
    local ln = vim.api.nvim_win_get_cursor(0)
    local fp = vim.fn.expand("%:p")

    print(vim.inspect(fp))
    print(vim.inspect(ln))

    dap.run({
        type = "mix_task",
        name = "mix phx.server",
        task = "test",
        taskArgs = { fp .. ":" .. ln[1], "--trace" },
        request = "launch",
        startApps = true, -- for Phoenix projects
        projectDir = "${workspaceFolder}",
        requireFiles = { "test/**/test_helper.exs", "test/**/*_test.exs" }
    })
end

vim.keymap.set("n", "t<C-d>", run_dap, { silent = true, noremap = true })

Keybind.g({
    { "n", "t<C-n>", ":TestNearest<CR>", { silent = true } },
    { "n", "t<C-f>", ":TestFile<CR>", { silent = true } },
    { "n", "t<C-s>", ":TestSuite<CR>", { silent = true } },
    { "n", "t<C-l>", ":TestLast<CR>", { silent = true } },
    { "n", "t<C-g>", ":TestVisit<CR>", { silent = true } }
})

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
