local lspconfig = require 'lspconfig'
local configs = require 'lspconfig/configs'
local util = require 'lspconfig/util'

local server_name = "tsserver"
local bin_name = "typescript-language-server"
if vim.fn.has('win32') == 1 then bin_name = bin_name .. ".cmd" end

local function organize_imports()
    local params = {
        command = "_typescript.organizeImports",
        arguments = {vim.api.nvim_buf_get_name(0)},
        title = ""
    }
    vim.lsp.buf.execute_command(params)
end

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.completion.completionItem.resolveSupport =
    {properties = {'documentation', 'detail', 'additionalTextEdits'}}
configs[server_name] = {
    default_config = {
        on_attach = require'nvim.plugins.lsp.config'.on_attach,
        capabilities = capabilities,
        cmd = {bin_name, "--stdio"},
        filetypes = {
            "javascript", "javascriptreact", "javascript.jsx", "typescript",
            "typescriptreact", "typescript.tsx"
        },
        root_dir = function(fname)
            return util.root_pattern("tsconfig.json")(fname) or
                       util.root_pattern("package.json", "jsconfig.json", ".git")(
                           fname);
        end,
        commands = {
            OrganizeImports = {
                organize_imports,
                description = "Organize Imports"
            }
        }
    },
    flags = {debounce_text_changes = 150}

}

lspconfig.tsserver.setup {}
