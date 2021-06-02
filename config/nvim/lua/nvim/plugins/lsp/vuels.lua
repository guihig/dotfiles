local lspconfig = require 'lspconfig'
local configs = require 'lspconfig/configs'
local util = require 'lspconfig/util'

local server_name = "vuels"
local bin_name = "vls"

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.completion.completionItem.resolveSupport =
    {properties = {'documentation', 'detail', 'additionalTextEdits'}}
configs[server_name] = {
    default_config = {
        on_attach = require'nvim.plugins.lsp.config'.on_attach,
        capabilities = capabilities,
        cmd = {bin_name},
        filetypes = {"vue"},
        root_dir = util.root_pattern("package.json"),
        init_options = {
            config = {
                vetur = {
                    useWorkspaceDependencies = true,
                    validation = {template = true, style = true, script = true},
                    completion = {
                        autoImport = true,
                        useScaffoldSnippets = true,
                        tagCasing = "kebab"
                    },
                    format = {
                        defaultFormatter = {js = "prettier", ts = "prettier"},
                        defaultFormatterOptions = {},
                        scriptInitialIndent = false,
                        styleInitialIndent = false
                    }
                },
                css = {},
                html = {suggest = {}},
                javascript = {format = {}},
                typescript = {format = {}},
                emmet = {},
                stylusSupremacy = {}
            }
        }
    }
}

lspconfig.vuels.setup {}
