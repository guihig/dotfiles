local lspconfig = require 'lspconfig'
local configs = require 'lspconfig/configs'
local util = require 'lspconfig/util'

local server_name = "cssls"
local bin_name = "vscode-css-language-server"

local root_pattern = util.root_pattern("package.json")

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)
configs[server_name] = {
    default_config = {
        on_attach = require'nvim.plugins.lsp.config'.on_attach,
        cmd = {bin_name, "--stdio"},
        filetypes = {"css", "scss", "less"},
        root_dir = function(fname)
            return root_pattern(fname) or vim.loop.os_homedir()
        end,
        settings = {
            css = {validate = true},
            scss = {validate = true},
            less = {validate = true}
        },
        capabilities = capabilities,
        flags = {debounce_text_changes = 500}
    }
}

lspconfig.cssls.setup {}
