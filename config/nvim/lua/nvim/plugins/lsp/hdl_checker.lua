local lspconfig = require 'lspconfig'
local configs = require 'lspconfig/configs'
local util = require 'lspconfig/util'

-- Check if it's already defined for when reloading this file.
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)
if not lspconfig.hdl_checker then
    configs.hdl_checker = {
        default_config = {
            on_attach = require'nvim.plugins.lsp.config'.on_attach,
            cmd = {'hdl_checker', '--lsp'},
            filetypes = {'vhdl', 'verilog', 'systemverilog'},
            root_dir = util.root_pattern("*.qpf", ".hdl_checker.config"),
            settings = {},
            capabilities = capabilities,
            flags = {debounce_text_changes = 150}

        }
    }
end
lspconfig.hdl_checker.setup {}
