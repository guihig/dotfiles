require("lspsaga").setup({
    symbol_in_winbar = { folder_level = 4 },
    code_action = { show_server_name = true, keys = { quit = "<ESC>" } },
    definition = { keys = { quit = "<ESC>" } },
    diagnostic = { max_height = 0.8, keys = { quit = "<ESC>" } },
    rename = { keys = { quit = "<ESC>" } }
})
