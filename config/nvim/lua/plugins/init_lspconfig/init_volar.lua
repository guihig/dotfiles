local configs = require "lspconfig.configs"
local lsp_util = require "lspconfig/util"

-- Volar
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
