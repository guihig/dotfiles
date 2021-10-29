local lspconfig = require 'lspconfig'
local configs = require 'lspconfig/configs'
local util = require 'lspconfig/util'
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

USER = vim.fn.expand('$USER')

-- HDL Checker
if not lspconfig.hdl_checker then
    configs.hdl_checker = {
        default_config = {
            on_attach = require'nvim.plugins.lsp.config'.on_attach,
            cmd = {'hdl_checker', '--lsp'},
            filetypes = {'vhdl', 'verilog', 'systemverilog'},
            root_dir = util.root_pattern("*.qpf", ".hdl_checker.config"),
            settings = {},
            capabilities = capabilities,
            flags = {debounce_text_changes = 150}

        }
    }
end
lspconfig.hdl_checker.setup {}

-- ElixirLS
local elixirls_binary = ""
elixirls_binary = "/home/" .. USER .. "/.lsp/elixir-ls/language_server.sh"
lspconfig.elixirls.setup {
    on_attach = require'nvim.plugins.lsp.config'.on_attach,
    cmd = {elixirls_binary},
    capabilities = capabilities
}

-- CSSLS
lspconfig.cssls.setup {
    on_attach = require'nvim.plugins.lsp.config'.on_attach,
    capabilities = capabilities
}

-- HLS
lspconfig.hls.setup {
    on_attach = require'nvim.plugins.lsp.config'.on_attach,
    capabilities = capabilities
}

-- JsonLS
lspconfig.jsonls.setup {
    on_attach = require'nvim.plugins.lsp.config'.on_attach,
    capabilities = capabilities
}

-- SumnekoLua
local sumneko_root_path = ""
local sumneko_binary = ""

sumneko_root_path = "/home/" .. USER .. "/.lsp/lua-language-server"
sumneko_binary = "/home/" .. USER ..
                     "/.lsp/lua-language-server/bin/Linux/lua-language-server"
lspconfig.sumneko_lua.setup {
    on_attach = require'nvim.plugins.lsp.config'.on_attach,
    capabilities = capabilities,
    cmd = {sumneko_binary, "-E", sumneko_root_path .. "/main.lua"},
    settings = {
        Lua = {
            runtime = {
                -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
                version = 'LuaJIT',
                -- Setup your lua path
                path = vim.split(package.path, ';')
            },
            diagnostics = {
                -- Get the language server to recognize the `vim` global
                globals = {'vim'}
            },
            workspace = {
                -- Make the server aware of Neovim runtime files
                library = {
                    [vim.fn.expand('$VIMRUNTIME/lua')] = true,
                    [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true
                }
            },
            -- Do not send telemetry data containing a randomized but unique identifier
            telemetry = {enable = false}
        }
    }
}

-- Tsserver
local function organize_imports()
    local params = {
        command = "_typescript.organizeImports",
        arguments = {vim.api.nvim_buf_get_name(0)},
        title = ""
    }
    vim.lsp.buf.execute_command(params)
end
lspconfig.tsserver.setup {
    on_attach = require'nvim.plugins.lsp.config'.on_attach,
    capabilities = capabilities,
    commands = {
        OrganizeImports = {organize_imports, description = "Organize Imports"}
    }
}

-- texlab
lspconfig.texlab.setup {
    on_attach = require'nvim.plugins.lsp.config'.on_attach,
    capabilities = capabilities,
    settings = {
        texlab = {
            build = {
                executable = 'lualatex',
                args = {
                    '-pdf', '-interaction=nonstopmode', '-synctex=1', '%f',
                    '-pvc'
                },
                onSave = true,
                forwardSearchAfter = false
            },
            auxDirectory = '.',
            forwardSearch = {
                executable = "zathura",
                args = {"--synctex-forward", "%l:1:%f", "%p"}
            },
            chktex = {onOpenAndSave = false, onEdit = false},
            diagnosticsDelay = 300,
            latexFormatter = 'latexindent',
            latexindent = {
                ['local'] = nil, -- local is a reserved keyword
                modifyLineBreaks = false
            },
            bibtexFormatter = 'texlab',
            formatterLineLength = 80
        }
    }
}

-- Volar
local function on_new_config(new_config, new_root_dir)
    local function get_typescript_server_path(root_dir)
        local project_root = util.find_node_modules_ancestor(root_dir)
        return project_root and
                   (util.path.join(project_root, 'node_modules', 'typescript',
                                   'lib', 'tsserverlibrary.js')) or ''
    end

    if new_config.init_options and new_config.init_options.typescript and
        new_config.init_options.typescript.serverPath == '' then
        new_config.init_options.typescript.serverPath =
            get_typescript_server_path(new_root_dir)
    end
end

local volar_cmd = {'volar-server', '--stdio'}
local volar_root_dir = util.root_pattern 'package.json'

configs.volar_api = {
    default_config = {
        cmd = volar_cmd,
        root_dir = volar_root_dir,
        on_new_config = on_new_config,
        filetypes = {'vue'},
        -- If you want to use Volar's Take Over Mode (if you know, you know)
        -- filetypes = { 'typescript', 'javascript', 'javascriptreact', 'typescriptreact', 'vue', 'json' },
        init_options = {
            typescript = {serverPath = ''},
            languageFeatures = {
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
                    defaultTagNameCase = 'both',
                    defaultAttrNameCase = 'kebabCase',
                    getDocumentNameCasesRequest = false,
                    getDocumentSelectionRequest = false
                }
            }
        }
    }
}
lspconfig.volar_api.setup {
    on_attach = require'nvim.plugins.lsp.config'.on_attach,
    capabilities = capabilities
}

configs.volar_doc = {
    default_config = {
        cmd = volar_cmd,
        root_dir = volar_root_dir,
        on_new_config = on_new_config,

        filetypes = {'vue'},
        -- If you want to use Volar's Take Over Mode (if you know, you know):
        -- filetypes = { 'typescript', 'javascript', 'javascriptreact', 'typescriptreact', 'vue', 'json' },
        init_options = {
            typescript = {serverPath = ''},
            languageFeatures = {
                documentHighlight = true,
                documentLink = true,
                codeLens = {showReferencesNotification = true},
                -- not supported - https://github.com/neovim/neovim/pull/14122
                semanticTokens = false,
                diagnostics = true,
                schemaRequestService = true
            }
        }
    }
}
lspconfig.volar_doc.setup {
    on_attach = require'nvim.plugins.lsp.config'.on_attach,
    capabilities = capabilities
}

configs.volar_html = {
    default_config = {
        cmd = volar_cmd,
        root_dir = volar_root_dir,
        on_new_config = on_new_config,

        filetypes = {'vue'},
        -- If you want to use Volar's Take Over Mode (if you know, you know), intentionally no 'json':
        -- filetypes = { 'typescript', 'javascript', 'javascriptreact', 'typescriptreact', 'vue' },
        init_options = {
            typescript = {serverPath = ''},
            documentFeatures = {
                selectionRange = true,
                foldingRange = true,
                linkedEditingRange = true,
                documentSymbol = true,
                -- not supported - https://github.com/neovim/neovim/pull/13654
                documentColor = false,
                documentFormatting = {defaultPrintWidth = 100}
            }
        }
    }
}
lspconfig.volar_html.setup {
    on_attach = require'nvim.plugins.lsp.config'.on_attach,
    capabilities = capabilities
}
