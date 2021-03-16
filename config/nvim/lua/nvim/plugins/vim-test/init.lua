Keybind.g({
    {'n', 't<C-n>', ':TestNearest', {silent = true}},
    {'n', 't<C-f>', ':TestFile', {silent = true}},
    {'n', 't<C-s>', ':TestSuite', {silent = true}},
    {'n', 't<C-l>', ':TestLast', {silent = true}},
    {'n', 't<C-g>', ':TestVisit', {silent = true}}
})

Variable.g({
    ["test#strategy"] = "dispatch",
    ["test#preserve_screen"] = 0
})
