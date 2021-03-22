local lspconfig = require 'lspconfig'
local configs = require 'lspconfig/configs'
local util = require 'lspconfig/util'

local server_name = "cssls"
local bin_name = "css-languageserver"

local root_pattern = util.root_pattern("package.json")

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
configs[server_name] = {
    default_config = {
        cmd = {bin_name, "--stdio"},
        capabilities = capabilities,
        filetypes = {"css", "scss", "less"},
        root_dir = function(fname)
            return root_pattern(fname) or vim.loop.os_homedir()
        end,
        settings = {
            css = {validate = true},
            scss = {validate = true},
            less = {validate = true}
        }
    },
    docs = {
        description = [[
https://github.com/vscode-langservers/vscode-css-languageserver-bin
`css-languageserver` can be installed via `npm`:
```sh
npm install -g vscode-css-languageserver-bin
```
]],
        default_config = {root_dir = [[root_pattern("package.json")]]}
    }
}

lspconfig.cssls.setup {}
