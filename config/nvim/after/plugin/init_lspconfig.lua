local keymap = vim.keymap
local lspconfig = require("lspconfig")
local lsp_zero = require("lsp-zero")
local mason_path = require("mason-core.path")
-- local utils = require("utils")

require("neodev").setup({
	override = function(root_dir, library)
		if root_dir:find("/etc/nixos", 1, true) == 1 then
			library.enabled = true
			library.plugins = true
		end
	end,
})

lsp_zero.on_attach(function(client, bufnr)
	local opts = { buffer = bufnr, remap = false }

	-- keymap.set("n", "gd", "<cmd>Lspsaga goto_definition<cr>", opts)
	keymap.set("n", "gd", vim.lsp.buf.definition, opts)
	keymap.set("n", "gr", "<cmd>Lspsaga finder<cr>", opts)
	keymap.set("n", "gi", "<cmd>Lspsaga peek_definition<cr>", opts)
	keymap.set("n", "gs", "<cmd>Lspsaga peek_type_definition<cr>", opts)
	keymap.set({ "n", "v" }, "<leader>a", "<cmd>Lspsaga code_action<cr>", opts)
	keymap.set("n", "K", "<cmd>Lspsaga hover_doc<cr>", opts)
	keymap.set("n", "<leader>e", "<cmd>Lspsaga show_cursor_diagnostics<cr>", opts)
	keymap.set("n", "]e", "<cmd>Lspsaga diagnostic_jump_next<cr>", opts)
	keymap.set("n", "[e", "<cmd>Lspsaga diagnostic_jump_prev<cr>", opts)
	keymap.set("n", "<leader>rn", "<cmd>Lspsaga rename<cr>", opts)
	keymap.set("n", "<leader>o", "<cmd>Lspsaga outline<cr>", opts)
end)

local tsdk = function()
	return vim.fn.getcwd() .. "/node_modules/typescript/lib"
end

local handlers = {
	lsp_zero.default_setup,
	["tsserver"] = function()
		require("lspconfig").tsserver.setup({
			filetypes = { "javascript", "javascriptreact", "typescript", "typescriptreact" },
			init_options = {
				typescript = {
					tsdk = tsdk(),
				},
			},
		})
	end,
	["elixirls"] = function()
		require("lspconfig").elixirls.setup({
			cmd = { mason_path.bin_prefix() .. "/elixir-ls" },
		})
	end,
	["jsonls"] = function()
		require("lspconfig").jsonls.setup({
			settings = {
				json = {
					schemas = require("schemastore").json.schemas({
						select = {
							"package.json",
							".eslintrc",
							"tsconfig.json",
						},
					}),
					validate = { enable = true },
				},
			},
		})
	end,
	["cssls"] = function()
		require("lspconfig").cssls.setup({
			filetypes = { "css", "scss", "less", "sass", "vue" },
			settings = {
				css = { validate = true, lint = {
					unknownAtRules = "ignore",
				} },
				scss = { validate = true, lint = {
					unknownAtRules = "ignore",
				} },
				less = { validate = true, lint = {
					unknownAtRules = "ignore",
				} },
			},
		})
	end,
	["volar"] = function()
		require("lspconfig").volar.setup({
			filetypes = { "vue" },
			init_options = {
				vue = {
					hybridMode = false,
				},
				typescript = {
					tsdk = tsdk(),
				},
			},
			-- filetypes = { "typescript", "javascript", "javascriptreact", "typescriptreact", "vue", "json" },
			-- capabilities = {
			-- 	workspace = {
			-- 		didChangeWatchedFiles = {
			-- 			dynamicRegistration = true,
			-- 		},
			-- 	},
			-- },
		})
	end,
}

require("mason").setup({
	PATH = "append",
})

require("mason-lspconfig").setup({
	ensure_installed = {
		"lua_ls",
		"dockerls",
		"elixirls",
		"jsonls",
		"tsserver",
		"eslint",
		"cssls",
		"pyright",
		"sqlls",
		"volar",
		"yamlls",
		"cssmodules_ls",
		"rust_analyzer",
		"nil_ls",
	},
	handlers = handlers,
})

-- Manual servers cfgs
local servers = {
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
						"-pvc",
					},
					onSave = true,
					forwardSearchAfter = false,
				},
				forwardSearch = {
					executable = "zathura",
					args = { "--synctex-forward", "%l:1:%f", "%p" },
				},
			},
		},
	},
}

for server, server_cfg in pairs(servers) do
	lspconfig[server].setup(server_cfg)
end
