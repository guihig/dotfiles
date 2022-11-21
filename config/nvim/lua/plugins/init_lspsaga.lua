local saga = require("lspsaga")

saga.init_lsp_saga({
    code_action_keys = { quit = "<Esc>", exec = "<CR>" },
    symbol_in_winbar = { in_custom = true }
})
