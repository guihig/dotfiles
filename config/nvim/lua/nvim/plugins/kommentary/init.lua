require('kommentary.config').configure_language("default", {
    prefer_single_line_comments = true,
    hook_function = function()
        require('ts_context_commentstring.internal').update_commentstring()
    end
})

vim.g.kommentary_create_default_mappings = false

vim.api.nvim_set_keymap("n", "<leader>c<space>",
                        "<Plug>kommentary_line_default", {})
vim.api
    .nvim_set_keymap("n", "<leader>cm", "<Plug>kommentary_motion_default", {})
vim.api.nvim_set_keymap("v", "<leader>c<space>",
                        "<Plug>kommentary_visual_default <Esc>", {})
