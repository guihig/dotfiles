local M = {}

local opts = { noremap = true, silent = true }
vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, opts)
vim.keymap.set("n", "[g", vim.diagnostic.goto_prev, opts)
vim.keymap.set("n", "]g", vim.diagnostic.goto_next, opts)

function _G.lsp_organize_imports()
    local context = { only = { "source.organizeImports" } }
    vim.lsp.buf.code_action(context)
end

M.on_attach = function(_, bufnr)
    local l_opts = { noremap = true, silent = true }
    Option.b(bufnr, { omnifunc = "v:lua.vim.lsp.omnifunc" })
    Keybind.b({
        { bufnr, "n", "gD", "<Cmd>lua vim.lsp.buf.declaration()<CR>", l_opts },
        { bufnr, "n", "gd", "<Cmd>lua vim.lsp.buf.definition()<CR>", l_opts },
        { bufnr, "n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", l_opts },
        { bufnr, "n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", l_opts },
        {
            bufnr,
            "n",
            "<leader>D",
            "<cmd>lua vim.lsp.buf.type_definition()<CR>",
            l_opts
        }, -- LspSaga
        { bufnr, "n", "<C-k>", "<cmd>Lspsaga signature_help<CR>", l_opts },
        { bufnr, "n", "K", "<cmd>Lspsaga hover_doc<CR>", l_opts },
        { bufnr, "n", "<leader>a", "<cmd>Lspsaga code_action<CR>", l_opts },
        { bufnr, "n", "<leader>rn", "<cmd>Lspsaga rename<CR>", l_opts },
        { bufnr, "n", "<leader>qq", "<cmd>Lspsaga lsp_finder<CR>", l_opts },
        { bufnr, "n", "<leader>k", "<cmd>Lspsaga preview_definition<CR>", l_opts },
        -- { bufnr, "n", "]g", "<cmd>Lspsaga diagnostic_jump_next<CR>", opts },
        -- { bufnr, "n", "[g", "<cmd>Lspsaga diagnostic_jump_prev<CR>", opts },
        -- {
        --     bufnr,
        --     "n",
        --     "<leader>e",
        --     "<cmd>Lspsaga show_line_diagnostics<CR>",
        --     opts
        -- }, 
        { bufnr, "n", "<C-A-o>", "<cmd>lua lsp_organize_imports()<CR>", l_opts }
    })
end

return M
