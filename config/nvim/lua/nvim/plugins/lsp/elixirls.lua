USER = vim.fn.expand('$USER')

local lspconfig = require 'lspconfig'
local configs = require 'lspconfig/configs'
local util = require 'lspconfig/util'

local server_name = "elixirls"

local elixirls_binary = ""

elixirls_binary = "/home/" .. USER .. "/.lsp/elixir-ls/language_server.sh"

configs[server_name] = {
    default_config = {
        on_attach = require'completion'.on_attach,
        cmd = {elixirls_binary},
        filetypes = {"elixir", "eelixir"},
        root_dir = function(fname)
            return util.root_pattern("mix.exs", ".git")(fname) or
                       vim.loop.os_homedir()
        end
    },
    docs = {
        package_json = "https://raw.githubusercontent.com/JakeBecker/vscode-elixir-ls/master/package.json",
        description = [[
https://github.com/elixir-lsp/elixir-ls
`elixir-ls` can be installed by following the instructions [here](https://github.com/elixir-lsp/elixir-ls#building-and-running).
```bash
curl -fLO https://github.com/elixir-lsp/elixir-ls/releases/latest/download/elixir-ls.zip
unzip elixir-ls.zip -d /path/to/elixir-ls
# Unix
chmod +x /path/to/elixir-ls/language_server.sh
```
**By default, elixir-ls doesn't have a `cmd` set.** This is because nvim-lspconfig does not make assumptions about your path. You must add the following to your init.vim or init.lua to set `cmd` to the absolute path ($HOME and ~ are not expanded) of your unzipped elixir-ls.
```lua
require'lspconfig'.elixirls.setup{
    -- Unix
    cmd = { "/path/to/elixir-ls/language_server.sh" };
    -- Windows
    cmd = { "/path/to/elixir-ls/language_server.bat" };
    ...
}
```
]],
        default_config = {
            root_dir = [[root_pattern("mix.exs", ".git") or vim.loop.os_homedir()]]
        }
    }
}

lspconfig.elixirls.setup {}