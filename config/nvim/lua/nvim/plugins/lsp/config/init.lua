local M = {}

M.on_attach = function(_, bufnr)
    local opts = {noremap = true, silent = true}
    Option.b(bufnr, {omnifunc = 'v:lua.vim.lsp.omnifunc'})
    Keybind.b({
        {bufnr, 'n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts},
        {bufnr, 'n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts},
        {bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts},
        {bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts},
        {
            bufnr, 'n', '<leader>D',
            '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts
        },
        {bufnr, 'n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts},
        -- {bufnr, 'n', 'K', '<cmd>Lspsaga hover_doc<CR>', opts},
        {
            bufnr, 'n', '<leader>a', '<cmd>lua vim.lsp.buf.code_action()<CR>',
            opts
        },
        {bufnr, 'n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts},
        {bufnr, 'n', ']g', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts},
        {bufnr, 'n', '[g', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts},
        {
            bufnr, 'n', '<leader>e',
            '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts
        }
    })

end

M.on_tsserver_attach = function(client, bufnr)
    local opts = {noremap = true, silent = true}
    Option.b(bufnr, {omnifunc = 'v:lua.vim.lsp.omnifunc'})
    Keybind.b({
        {bufnr, 'n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts},
        {bufnr, 'n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts},
        {bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts},
        {bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts},
        {
            bufnr, 'n', '<leader>D',
            '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts
        },
        {bufnr, 'n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts},
        {bufnr, 'n', 'K', '<cmd>Lspsaga hover_doc<CR>', opts},
        {
            bufnr, 'n', '<leader>a', '<cmd>lua vim.lsp.buf.code_action()<CR>',
            opts
        },
        {bufnr, 'n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts},
        {bufnr, 'n', ']g', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts},
        {bufnr, 'n', '[g', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts},
        {
            bufnr, 'n', '<leader>e',
            '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts
        }, -- Custom
        {bufnr, 'n', '<C-A-o>', '<cmd>OrganizeImports<CR>:update<CR>', opts}
    })

    if client.config.flags then
        client.config.flags.allow_incremental_sync = true
    end
    client.server_capabilities.completionProvider.triggerCharacters =
        {
            ".", '"', "'", "`", "/", "@", "*", "#", "$", "+", "^", "(", "[",
            "-", ":"
        }
end

return M
