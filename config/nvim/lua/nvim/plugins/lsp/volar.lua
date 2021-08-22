local lspconfig = require 'lspconfig'
local configs = require 'lspconfig/configs'
local util = require 'lspconfig/util'

local server_name = "volar"
local bin = {
    "node", "/home/ferreira/dev/builds/volar/packages/server/out/index.js",
    "--stdio"
}

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.resolveSupport =
    {properties = {'documentation', 'detail', 'additionalTextEdits'}}
configs[server_name] = {
    default_config = {
        on_attach = require'nvim.plugins.lsp.config'.on_attach,
        capabilities = capabilities,
        cmd = bin,
        filetypes = {"vue"},
        root_dir = util.root_pattern("package.json"),
        flags = {debounce_text_changes = 150}
    }
}

lspconfig.volar.setup {}
