local action, hover = require("lspsaga.action"), require("lspsaga.hover")
local M = {}

-- M.on_attach = function(_, bufnr)
--     local opts = {noremap = true, silent = true}
--     Option.b(bufnr, {omnifunc = 'v:lua.vim.lsp.omnifunc'})
--     Keybind.b({
--         {bufnr, 'n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts},
--         {bufnr, 'n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts},
--         {bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts},
--         {bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts},
--         {
--             bufnr, 'n', '<leader>D',
--             '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts
--         },
--         {bufnr, 'n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts},
--         {bufnr, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts},
--         {
--             bufnr, 'n', '<leader>a', '<cmd>lua vim.lsp.buf.code_action()<CR>',
--             opts
--         },
--         {bufnr, 'n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts},
--         {bufnr, 'n', ']g', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts},
--         {bufnr, 'n', '[g', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts},
--         {
--             bufnr, 'n', '<leader>e',
--             '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts
--         }, {bufnr, 'n', '<C-A-o>', '<cmd>OrganizeImports<CR>:update<CR>', opts}
--     })

-- end

-- M.on_tsserver_attach = function(client, bufnr)
--     local opts = {noremap = true, silent = true}
--     Option.b(bufnr, {omnifunc = 'v:lua.vim.lsp.omnifunc'})
--     Keybind.b({
--         {bufnr, 'n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts},
--         {bufnr, 'n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts},
--         {bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts},
--         {bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts},
--         {
--             bufnr, 'n', '<leader>D',
--             '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts
--         },
--         {bufnr, 'n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts},
--         {bufnr, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts},
--         {
--             bufnr, 'n', '<leader>a', '<cmd>lua vim.lsp.buf.code_action()<CR>',
--             opts
--         },
--         {bufnr, 'n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts},
--         {bufnr, 'n', ']g', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts},
--         {bufnr, 'n', '[g', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts},
--         {
--             bufnr, 'n', '<leader>e',
--             '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts
--         }, -- Custom
--         {bufnr, 'n', '<C-A-o>', '<cmd>OrganizeImports<CR>:update<CR>', opts}
--     })

--     if client.config.flags then
--         client.config.flags.allow_incremental_sync = true
--     end
--     client.server_capabilities.completionProvider.triggerCharacters =
--         {
--             ".", '"', "'", "`", "/", "@", "*", "#", "$", "+", "^", "(", "[",
--             "-", ":"
--         }
-- end

local function t(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end

function _G.LspSagaSmartScrollDown()
    if hover.has_saga_hover() then
        return action.smart_scroll_with_saga(1)
    else
        return t "<Tab>"
    end
end

function _G.LspSagaSmartScrollUp()
    if hover.has_saga_hover() then
        return action.smart_scroll_with_saga(-1)
    else
        return t "<S-Tab>"
    end
end

M.on_attach = function(_, bufnr)
    local opts = {noremap = true, silent = true}
    local expr_opts = {noremap = true, silent = true, expr = true}
    Option.b(bufnr, {omnifunc = 'v:lua.vim.lsp.omnifunc'})
    Keybind.b({
        {bufnr, 'n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts},
        {bufnr, 'n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts},
        {bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts},
        {bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts},
        {bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts},
        {bufnr, 'n', '<Tab>', 'v:lua.LspSagaSmartScrollDown()', expr_opts},
        {bufnr, 'n', '<S-Tab>', 'v:lua.LspSagaSmartScrollUp()', expr_opts},
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
