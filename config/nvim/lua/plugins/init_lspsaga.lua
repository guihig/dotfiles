local saga = require("lspsaga")

saga.setup({
    code_action = {
        num_shortcut = true,
        keys = { quit = "<Esc>", exec = "<CR>" }
    },
    symbol_in_winbar = { enable = false }
})
