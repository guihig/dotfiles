local lspconfig = require 'lspconfig'
-- local configs = require 'lspconfig/configs'
local util = require 'lspconfig/util'
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

USER = vim.fn.expand('$USER')

-- HDL Checker
-- if not lspconfig.hdl_checker then
--     configs.hdl_checker = {
--         default_config = {
--             on_attach = require'nvim.plugins.lsp.config'.on_attach,
--             cmd = {'hdl_checker', '--lsp'},
--             filetypes = {'vhdl', 'verilog', 'systemverilog'},
--             root_dir = util.root_pattern("*.qpf", ".hdl_checker.config"),
--             settings = {},
--             capabilities = capabilities,
--             flags = {debounce_text_changes = 150}

--         }
--     }
-- end
-- lspconfig.hdl_checker.setup {}

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
-- local function on_new_config(new_config, new_root_dir)
--     local function get_typescript_server_path(root_dir)
--         local project_root = util.find_node_modules_ancestor(root_dir)
--         return project_root and
--                    (util.path.join(project_root, 'node_modules', 'typescript',
--                                    'lib', 'tsserverlibrary.js')) or ''
--     end

--     if new_config.init_options and new_config.init_options.typescript and
--         new_config.init_options.typescript.serverPath == '' then
--         new_config.init_options.typescript.serverPath =
--             get_typescript_server_path(new_root_dir)
--     end
-- end

lspconfig.volar.setup {
    on_attach = require'nvim.plugins.lsp.config'.on_attach,
    capabilities = capabilities,
    filetypes = {'vue'}
    -- on_new_config = on_new_config
}

-- Vetur
-- lspconfig.vuels.setup {
--     on_attach = require'nvim.plugins.lsp.config'.on_attach,
--     capabilities = capabilities,
--     filetypes = {'vue'}
-- }

-- Pyright
lspconfig.pyright.setup {
    on_attach = require'nvim.plugins.lsp.config'.on_attach,
    capabilities = capabilities
}
