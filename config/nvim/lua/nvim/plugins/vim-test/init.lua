Keybind.g({
    {'n', 't<C-n>', ':TestNearest<CR>', {silent = true}},
    {'n', 't<C-f>', ':TestFile<CR>', {silent = true}},
    {'n', 't<C-s>', ':TestSuite<CR>', {silent = true}},
    {'n', 't<C-l>', ':TestLast<CR>', {silent = true}},
    {'n', 't<C-g>', ':TestVisit<CR>', {silent = true}}
})

Variable.g({
    ["test#strategy"] = "dispatch",
    ["test#preserve_screen"] = 0
})
