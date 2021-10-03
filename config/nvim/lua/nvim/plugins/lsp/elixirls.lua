USER = vim.fn.expand('$USER')

local lspconfig = require 'lspconfig'
local configs = require 'lspconfig/configs'
local util = require 'lspconfig/util'
local server_name = "elixirls"

local elixirls_binary = ""

elixirls_binary = "/home/" .. USER .. "/.lsp/elixir-ls/language_server.sh"

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)
configs[server_name] = {
    default_config = {
        on_attach = require'nvim.plugins.lsp.config'.on_attach,
        cmd = {elixirls_binary},
        filetypes = {"elixir", "eelixir"},
        root_dir = function(fname)
            return util.root_pattern("mix.exs", ".git")(fname) or
                       vim.loop.os_homedir()
        end,
        capabilities = capabilities,
        flags = {debounce_text_changes = 500}
    }
}

lspconfig.elixirls.setup {}
