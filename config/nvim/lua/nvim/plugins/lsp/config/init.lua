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
        }, -- LspSaga
        {bufnr, 'n', '<C-k>', '<cmd>Lspsaga signature_help<CR>', opts},
        {bufnr, 'n', 'K', '<cmd>Lspsaga hover_doc<CR>', opts},
        {bufnr, 'n', '<leader>a', '<cmd>Lspsaga code_action<CR>', opts},
        {bufnr, 'n', '<leader>rn', '<cmd>Lspsaga rename<CR>', opts},
        {bufnr, 'n', '<leader>qq', '<cmd>Lspsaga lsp_finder<CR>', opts},
        {bufnr, 'n', '<leader>k', '<cmd>Lspsaga preview_definition<CR>', opts},
        {bufnr, 'n', ']g', '<cmd>Lspsaga diagnostic_jump_next<CR>', opts},
        {bufnr, 'n', '[g', '<cmd>Lspsaga diagnostic_jump_prev<CR>', opts},
        {
            bufnr, 'n', '<leader>e', '<cmd>Lspsaga show_line_diagnostics<CR>',
            opts
        }, -- Custom
        {bufnr, 'n', '<C-A-o>', '<cmd>OrganizeImports<CR>:update<CR>', opts}
    })
end

return M
