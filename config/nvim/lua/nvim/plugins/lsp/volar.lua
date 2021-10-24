local lspconfig = require 'lspconfig'
local configs = require 'lspconfig/configs'
local util = require 'lspconfig/util'

local function get_typescript_server_path(root_dir)
    local project_root = util.find_node_modules_ancestor(root_dir)
    return project_root and
               (util.path.join(project_root, 'node_modules', 'typescript',
                               'lib', 'tsserverlibrary.js')) or ''
end

local server_name = 'volar'
local bin_name = 'volar-server'

-- https://github.com/johnsoncodehk/volar/blob/master/packages/shared/src/types.ts
local volar_init_options = {
    typescript = {serverPath = ''},
    languageFeatures = {
        -- not supported - https://github.com/neovim/neovim/pull/14122
        semanticTokens = false,
        references = true,
        definition = true,
        typeDefinition = true,
        callHierarchy = true,
        hover = true,
        rename = true,
        renameFileRefactoring = true,
        signatureHelp = true,
        codeAction = true,
        completion = {
            defaultTagNameCase = 'both',
            defaultAttrNameCase = 'kebabCase'
        },
        schemaRequestService = true,
        documentHighlight = true,
        documentLink = true,
        codeLens = false,
        diagnostics = true
    },
    documentFeatures = {
        -- not supported - https://github.com/neovim/neovim/pull/13654
        documentColor = false,
        selectionRange = true,
        foldingRange = false,
        linkedEditingRange = true,
        documentSymbol = true,
        documentFormatting = {defaultPrintWidth = 100}
    }
}

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)
configs[server_name] = {
    default_config = {
        on_attach = require'nvim.plugins.lsp.config'.on_attach,
        cmd = {bin_name, '--stdio'},
        filetypes = {'vue'},
        root_dir = util.root_pattern 'package.json',
        capabilities = capabilities,
        init_options = volar_init_options,
        on_new_config = function(new_config, new_root_dir)
            if new_config.init_options and new_config.init_options.typescript and
                new_config.init_options.typescript.serverPath == '' then
                new_config.init_options.typescript.serverPath =
                    get_typescript_server_path(new_root_dir)
            end
        end,
        flags = {debounce_text_changes = 150}
    }
}

lspconfig.volar.setup {}
