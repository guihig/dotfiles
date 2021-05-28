local M = {}

-- Option.g({omnifunc = 'v:lua.vim.lsp.omnifunc'})

M.on_attach = function(_, bufnr)
    local opts = {noremap = true, silent = true}
    Option.b(bufnr, {omnifunc = 'v:lua.vim.lsp.omnifunc'})
    Keybind.b({
        {bufnr, 'n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts},
        {bufnr, 'n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts},
        -- {'n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts},
        {bufnr, 'n', 'K', '<cmd>Lspsaga hover_doc<CR>', opts},
        {bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts},
        -- {'n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts},
        {bufnr, 'n', '<C-k>', '<cmd>Lspsaga signature_help<CR>', opts}, {
            bufnr, 'n', '<leader>wa',
            '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts
        }, {
            bufnr, 'n', '<leader>wr',
            '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts
        }, {
            bufnr, 'n', '<leader>wl',
            '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>',
            opts
        },
        {
            bufnr, 'n', '<leader>D',
            '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts
        }, {bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts},
        -- {'n', '<leader>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts},
        {
            bufnr, 'n', '<leader>e', '<cmd>Lspsaga show_line_diagnostics<CR>',
            opts
        }, {bufnr, 'n', '[g', '<cmd>Lspsaga diagnostic_jump_prev<CR>', opts},
        {bufnr, 'n', ']g', '<cmd>Lspsaga diagnostic_jump_next<CR>', opts}, {
            bufnr, 'n', '<leader>q',
            '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts
        }, {bufnr, 'n', '<leader>a', '<cmd>Lspsaga code_action<CR>', opts},
        {bufnr, 'n', '<leader>rn', '<cmd>Lspsaga rename<CR>', opts},
        {bufnr, 'n', '<leader>qq', '<cmd>Lspsaga lsp_finder<CR>', opts},
        {bufnr, 'n', '<leader>f', '<cmd>Format<CR>:update<CR>', opts},
        {bufnr, 'n', '<C-A-o>', '<cmd>OrganizeImports<CR>:update<CR>', opts}
    })
end

-- Keybind.g({
--     {'n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts},
--     {'n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts},
--     -- {'n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts},
--     {'n', 'K', '<cmd>Lspsaga hover_doc<CR>', opts},
--     {'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts},
--     -- {'n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts},
--     {'n', '<C-k>', '<cmd>Lspsaga signature_help<CR>', opts},
--     {'n', '<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts},
--     {
--         'n', '<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>',
--         opts
--     }, {
--         'n', '<leader>wl',
--         '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>',
--         opts
--     }, {'n', '<leader>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts},
--     {'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts},
--     -- {'n', '<leader>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts},
--     {'n', '<leader>e', '<cmd>Lspsaga show_line_diagnostics<CR>', opts},
--     {'n', '[g', '<cmd>Lspsaga diagnostic_jump_prev<CR>', opts},
--     {'n', ']g', '<cmd>Lspsaga diagnostic_jump_next<CR>', opts},
--     {'n', '<leader>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts},
--     {'n', '<leader>a', '<cmd>Lspsaga code_action<CR>', opts},
--     {'n', '<leader>rn', '<cmd>Lspsaga rename<CR>', opts},
--     {'n', '<leader>qq', '<cmd>Lspsaga lsp_finder<CR>', opts},
--     {'n', '<leader>f', '<cmd>Format<CR>:update<CR>', opts},
--     {'n', '<C-A-o>', '<cmd>OrganizeImports<CR>:update<CR>', opts}
-- })

-- vim.api.nvim_exec([[
--   hi LspReferenceRead cterm=bold ctermbg=red guibg=LightYellow
--   hi LspReferenceText cterm=bold ctermbg=red guibg=LightYellow
--   hi LspReferenceWrite cterm=bold ctermbg=red guibg=LightYellow
--   augroup lsp_document_highlight
--     autocmd! * <buffer>
--     autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
--     autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
--   augroup END
-- ]], false)

return M
