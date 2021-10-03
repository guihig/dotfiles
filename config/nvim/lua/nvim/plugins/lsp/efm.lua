local eslint = {
    lintCommand = "yarn eslint -f unix --stdin --stdin-filename ${INPUT}",
    lintIgnoreExitCode = true,
    lintStdin = true,
    lintFormats = {"%f:%l:%c: %m"}
}

local credo = {
    lintCommand = "mix credo suggest --strict --format=flycheck --read-from-stdin ${INPUT}",
    lintIgnoreExitCode = true,
    lintStdin = true,
    lintFormats = {"%f:%l:%c: %m"},
    rootMarkers = {"mix.exs", "mix.lock"}
}

local languages = {
    typescript = {eslint},
    javascript = {eslint},
    typescriptreact = {eslint},
    javascriptreact = {eslint},
    vue = {eslint},
    elixir = {credo}
}

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)
require"lspconfig".efm.setup {
    on_attach = require'nvim.plugins.lsp.config'.on_attach,
    capabilities = capabilities,
    root_dir = require"lspconfig".util.root_pattern("yarn.lock", "lerna.json",
                                                    ".git"),
    init_options = {documentFormatting = true, codeAction = true},
    filetypes = vim.tbl_keys(languages),
    settings = {
        rootMarkers = {".git/"},
        languages = languages,
        lintDebounce = 1000000000
    },
    flags = {debounce_text_changes = 150}
}
