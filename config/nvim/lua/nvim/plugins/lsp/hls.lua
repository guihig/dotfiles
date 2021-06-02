local lspconfig = require 'lspconfig'
local configs = require 'lspconfig/configs'
local util = require 'lspconfig/util'

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.completion.completionItem.resolveSupport =
    {properties = {'documentation', 'detail', 'additionalTextEdits'}}
configs.hls = {
    default_config = {
        on_attach = require'nvim.plugins.lsp.config'.on_attach,
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
    }
};

lspconfig.hls.setup {}
