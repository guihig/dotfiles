local keymap = vim.keymap
local lspconfig = require "lspconfig"
local utils = require("utils")
local configs = require "lspconfig.configs"
local lsp_util = require "lspconfig/util"
local capabilities = require("cmp_nvim_lsp").default_capabilities()

-- Setup Volar Multiple svrs cfg
local function on_new_config(new_config, new_root_dir)
    local function get_typescript_server_path(root_dir)
        local project_root = lsp_util.find_node_modules_ancestor(root_dir)
        return project_root
                   and (lsp_util.path
                       .join(project_root, "node_modules", "typescript", "lib",
                             "tsserverlibrary.js")) or ""
    end

    if new_config.init_options and new_config.init_options.typescript
        and new_config.init_options.typescript.serverPath == "" then
        new_config.init_options.typescript.serverPath =
            get_typescript_server_path(new_root_dir)
    end
end

local volar_cmd = { "vue-language-server", "--stdio" }
local volar_root_dir = lsp_util.root_pattern "package.json"

if not configs.volar_api then
    configs.volar_api = {
        default_config = {
            cmd = volar_cmd,
            root_dir = volar_root_dir,
            on_new_config = on_new_config,
            filetypes = { "vue" },
            init_options = {
                typescript = { serverPath = "" },
                languageFeatures = {
                    implementation = true,
                    references = true,
                    definition = true,
                    typeDefinition = true,
                    callHierarchy = true,
                    hover = true,
                    rename = true,
                    renameFileRefactoring = true,
                    signatureHelp = true,
                    codeAction = true,
                    workspaceSymbol = true,
                    completion = {
                        defaultTagNameCase = "both",
                        defaultAttrNameCase = "kebabCase",
                        getDocumentNameCasesRequest = false,
                        getDocumentSelectionRequest = false
                    }
                }
            }
        }
    }
end

if not configs.volar_doc then
    configs.volar_doc = {
        default_config = {
            cmd = volar_cmd,
            root_dir = volar_root_dir,
            on_new_config = on_new_config,
            filetypes = { "vue" },
            init_options = {
                typescript = { serverPath = "" },
                languageFeatures = {
                    implementation = true,
                    documentHighlight = true,
                    documentLink = true,
                    codeLens = false,
                    semanticTokens = false,
                    diagnostics = true,
                    schemaRequestService = true
                }
            }
        }
    }
end

if not configs.volar_html then
    configs.volar_html = {
        default_config = {
            cmd = volar_cmd,
            root_dir = volar_root_dir,
            on_new_config = on_new_config,
            filetypes = { "vue" },
            init_options = {
                typescript = { serverPath = "" },
                documentFeatures = {
                    selectionRange = true,
                    foldingRange = true,
                    linkedEditingRange = true,
                    documentSymbol = true,
                    documentColor = false,
                    documentFormatting = { defaultPrintWidth = 100 }
                }
            }
        }
    }
end

-- Helpers
local function lsp_organize_imports()
    local context = { only = { "source.organizeImports" } }
    vim.lsp.buf.code_action(context)
end

FLOAT_WINID = nil
local function close_diag_float()
    if FLOAT_WINID ~= nil then
        pcall(vim.api.nvim_win_close, FLOAT_WINID, true)
        FLOAT_WINID = nil
        return
    else
        return utils.t "<Esc>"
    end
end

local function open_diag_float()
    local _, winid = vim.diagnostic.open_float()
    FLOAT_WINID = winid
end

local keymap_opts = { noremap = true, silent = true }
keymap.set("n", "<Esc>", close_diag_float, keymap_opts)
keymap.set("n", "<leader>e", open_diag_float, keymap_opts)
keymap.set("n", "[g", vim.diagnostic.goto_prev, keymap_opts)
keymap.set("n", "]g", vim.diagnostic.goto_next, keymap_opts)

local function on_attach(_, bufnr)
    vim.bo[bufnr].omnifunc = "v:lua.vim.lsp.omnifunc"

    local bufopts = { noremap = true, silent = true, buffer = bufnr }
    keymap.set("n", "gD", vim.lsp.buf.declaration, bufopts)
    keymap.set("n", "gd", vim.lsp.buf.definition, bufopts)
    keymap.set("n", "gi", vim.lsp.buf.implementation, bufopts)
    keymap.set("n", "gr", vim.lsp.buf.references, bufopts)
    keymap.set("n", "K", vim.lsp.buf.hover, bufopts)
    keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, bufopts)
    keymap.set("n", "<leader>D", vim.lsp.buf.type_definition, bufopts)
    keymap.set("n", "K", vim.lsp.buf.hover, bufopts)
    keymap.set("n", "<leader>a", vim.lsp.buf.code_action, bufopts)
    keymap.set("n", "<leader>rn", vim.lsp.buf.rename, bufopts)
    keymap.set("n", "<C-A-o>", lsp_organize_imports, bufopts)

end

-- Init
vim.cmd [[highlight NormalFloat guibg=none]]
vim.cmd [[highlight FloatBorder guifg=white guibg=none]]
-- vim.lsp.set_log_level("debug")

local border = {
    { "╭", "FloatBorder" },
    { "─", "FloatBorder" },
    { "╮", "FloatBorder" },
    { "│", "FloatBorder" },
    { "╯", "FloatBorder" },
    { "─", "FloatBorder" },
    { "╰", "FloatBorder" },
    { "│", "FloatBorder" }
}

-- To instead override globally
local orig_util_open_floating_preview = vim.lsp.util.open_floating_preview
function vim.lsp.util.open_floating_preview(contents, syntax, opts, ...)
    opts = opts or {}
    opts.border = opts.border or border
    return orig_util_open_floating_preview(contents, syntax, opts, ...)
end

lspconfig.util.default_config = vim.tbl_extend("force",
                                               lspconfig.util.default_config,
                                               { autostart = true })

-- Servers cfgs
local servers = {
    elixirls = {},
    cssls = {
        single_file_support = false,
        settings = {
            css = { validate = true },
            scss = { validate = false },
            less = { validate = true }
        }
    },
    tailwindcss = {
        filetypes = {
            "aspnetcorerazor",
            "astro",
            "astro-markdown",
            "blade",
            "django-html",
            "htmldjango",
            "edge",
            "ejs",
            "erb",
            "eruby",
            "gohtml",
            "haml",
            "handlebars",
            "hbs",
            "html",
            "html-eex",
            "heex",
            "jade",
            "leaf",
            "liquid",
            "markdown",
            "mdx",
            "mustache",
            "njk",
            "nunjucks",
            "php",
            "razor",
            "slim",
            "twig",
            "css",
            "less",
            "postcss",
            "sass",
            "scss",
            "stylus",
            "sugarss",
            "javascript",
            "javascriptreact",
            "reason",
            "rescript",
            "typescript",
            "typescriptreact",
            "vue",
            "svelte"
        }
    },
    hls = {},
    jsonls = {},
    sumneko_lua = {
        settings = {
            Lua = {
                completion = { autoRequire = true },
                runtime = {
                    -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
                    version = "LuaJIT"
                },
                diagnostics = {
                    -- Get the language server to recognize the `vim` global
                    globals = {
                        "vim",
                        -- Awesome Globals
                        "client",
                        "awesome",
                        "tag",
                        "root",
                        "screen",
                        "mouse"
                    },
                    unusedLocalExclude = { "_%s" }
                },
                workspace = {
                    -- Make the server aware of Neovim runtime files
                    library = vim.api.nvim_get_runtime_file("", true)
                },
                -- Do not send telemetry data containing a randomized but unique identifier
                telemetry = { enable = false }
            }
        }
    },
    tsserver = { flags = { debounce_text_changes = 50 } },
    texlab = {
        settings = {
            texlab = {
                build = {
                    executable = "lualatex",
                    args = {
                        "-pdf",
                        "-interaction=nonstopmode",
                        "-synctex=1",
                        "%f",
                        "-pvc"
                    },
                    onSave = true,
                    forwardSearchAfter = false
                },
                forwardSearch = {
                    executable = "zathura",
                    args = { "--synctex-forward", "%l:1:%f", "%p" }
                }
            }
        }
    },
    pyright = {},
    efm = {
        filetypes = {
            "javascript",
            "javascriptreact",
            "typescript",
            "typescriptreact",
            "vue",
            "elixir"
        }
    },
    volar_api = {},
    volar_doc = {},
    volar_html = {},
    yamlls = {},
    asm_lsp = {},
    rust_analyzer = {},
    clangd = {}
}

for server, server_cfg in pairs(servers) do
    local cfg = { on_attach = on_attach, capabilities = capabilities }
    local merged_cfg = utils.merge_table(cfg, server_cfg)
    lspconfig[server].setup(merged_cfg)
end
