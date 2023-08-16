require("LspUI").setup({
    code_action = {
        enable = true,
        command_enable = true,
        gitsigns = true,
        key_binding = { exec = "<CR>", prev = "k", next = "j", quit = "<Esc>" }
    }
})
