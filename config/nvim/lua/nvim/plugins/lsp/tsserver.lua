local lspconfig = require 'lspconfig'
local configs = require 'lspconfig/configs'
local util = require 'lspconfig/util'

local server_name = "tsserver"
local bin_name = "typescript-language-server"
if vim.fn.has('win32') == 1 then bin_name = bin_name .. ".cmd" end

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
configs[server_name] = {
    filetypes = {
        "javascript", "javascriptreact", "javascript.jsx", "typescript",
        "typescriptreact", "typescript.tsx"
    },
    default_config = {
        -- on_attach = require'completion'.on_attach,
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
        end
    },
    docs = {
        description = [[
https://github.com/theia-ide/typescript-language-server
`typescript-language-server` depends on `typescript`. Both packages can be installed via `npm`:
```sh
npm install -g typescript typescript-language-server
```
]],
        default_config = {
            root_dir = [[root_pattern("package.json", "tsconfig.json", "jsconfig.json", ".git")]]
        }
    }
}

lspconfig.tsserver.setup {}
