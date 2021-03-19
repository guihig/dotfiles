local lspconfig = require 'lspconfig'
local configs = require 'lspconfig/configs'
local util = require 'lspconfig/util'

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
configs.hls = {
    default_config = {
        cmd = {"haskell-language-server-wrapper", "--lsp"},
        capabilities = capabilities,
        filetypes = {"haskell", "lhaskell"},
        root_dir = util.root_pattern("*.cabal", "stack.yaml", "cabal.project",
                                     "package.yaml", "hie.yaml"),
        settings = {languageServerHaskell = {formattingProvider = "ormolu"}},
        lspinfo = function(cfg)
            -- return "specific"
            if cfg.settings.languageServerHaskell.logFile or false then
                return "logfile: " .. cfg.settings.languageServerHaskell.logFile
            end
            return ""
        end
    },

    docs = {
        description = [[
https://github.com/haskell/haskell-language-server
Haskell Language Server
        ]],

        default_config = {
            root_dir = [[root_pattern("*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml")]]
        }
    }
};

lspconfig.hls.setup {}
