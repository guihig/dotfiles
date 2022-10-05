local lspconfig = require "lspconfig"
local utils = require("utils")
local helpers = require("plugins.init_lspconfig.lsp_helpers")
local capabilities = require("cmp_nvim_lsp").update_capabilities(vim.lsp
                                                                     .protocol
                                                                     .make_client_capabilities())
require("plugins.init_lspconfig.init_volar")

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
    volar_html = {}
}

for server, server_cfg in pairs(servers) do
    local cfg = { on_attach = helpers.on_attach, capabilities = capabilities }
    local merged_cfg = utils.merge_table(cfg, server_cfg)
    lspconfig[server].setup(merged_cfg)
end
