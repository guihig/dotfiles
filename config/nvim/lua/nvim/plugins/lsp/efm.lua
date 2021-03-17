local eslint = {
    lintCommand = "eslint -f unix --stdin --stdin-filename ${INPUT}",
    lintIgnoreExitCode = true,
    lintStdin = true,
    lintFormats = {"%f:%l:%c: %m"}
}

local credo = {
    lintCommand = "mix credo suggest --format=flycheck --read-from-stdin ${INPUT}",
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
    vue = {eslint}
}

require"lspconfig".efm.setup {
    on_attach = require'completion'.on_attach,
    root_dir = require"lspconfig".util.root_pattern("yarn.lock", "lerna.json",
                                                    ".git"),
    init_options = {documentFormatting = true, codeAction = true},
    filetypes = vim.tbl_keys(languages),
    settings = {
        rootMarkers = {".git/"},
        languages = {
            javascript = {eslint},
            typescript = {eslint},
            vue = {eslint},
            elixir = {credo}
        }
    }
}
