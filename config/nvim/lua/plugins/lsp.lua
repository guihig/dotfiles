local keymap = vim.keymap

return {
	{
		"nvimdev/lspsaga.nvim",
		opts = {
			symbol_in_winbar = { folder_level = 4 },
			code_action = { show_server_name = true, keys = { quit = "<ESC>" } },
			definition = { keys = { quit = "<ESC>" } },
			diagnostic = { max_height = 0.8, keys = { quit = "<ESC>" } },
			rename = { keys = { quit = "<ESC>" } },
			lightbulb = {
				sign = false,
				virtual_text = true,
			},
		},
		config = function(_, opts)
			require("lspsaga").setup(opts)
		end,
	},
	{
		"neovim/nvim-lspconfig",
		dependencies = {
			{ "VonHeikemen/lsp-zero.nvim", branch = "v4.x" },
			{ "williamboman/mason.nvim" },
			{ "williamboman/mason-lspconfig.nvim" },
		},
		config = function()
			local lsp_zero = require("lsp-zero")
			local mason_path = require("mason-core.path")
			local mason_packages = vim.fn.stdpath("data") .. "/mason/packages"
			local volar_path = mason_packages .. "/vue-language-server/node_modules/@vue/language-server"

			local lsp_attach = function(_, bufnr)
				local keymap_opts = { buffer = bufnr, remap = false }

				-- keymap.set("n", "gd", "<cmd>Lspsaga goto_definition<cr>", opts)
				keymap.set("n", "gd", vim.lsp.buf.definition, keymap_opts)
				keymap.set("n", "gr", "<cmd>Lspsaga finder<cr>", keymap_opts)
				keymap.set("n", "gi", "<cmd>Lspsaga peek_definition<cr>", keymap_opts)
				keymap.set("n", "gs", "<cmd>Lspsaga peek_type_definition<cr>", keymap_opts)
				keymap.set({ "n", "v" }, "<leader>a", "<cmd>Lspsaga code_action<cr>", keymap_opts)
				keymap.set("n", "K", "<cmd>Lspsaga hover_doc<cr>", keymap_opts)
				keymap.set("n", "<leader>e", "<cmd>Lspsaga show_cursor_diagnostics<cr>", keymap_opts)
				keymap.set("n", "]e", "<cmd>Lspsaga diagnostic_jump_next<cr>", keymap_opts)
				keymap.set("n", "[e", "<cmd>Lspsaga diagnostic_jump_prev<cr>", keymap_opts)
				keymap.set("n", "<leader>rn", "<cmd>Lspsaga rename<cr>", keymap_opts)
				keymap.set("n", "<leader>o", "<cmd>Lspsaga outline<cr>", keymap_opts)
			end

			lsp_zero.extend_lspconfig({
				sign_text = true,
				lsp_attach = lsp_attach,
			})

			local handlers = {
				function(server_name)
					require("lspconfig")[server_name].setup({})
				end,
				["tsserver"] = function()
					require("lspconfig").tsserver.setup({
						filetypes = { "javascript", "javascriptreact", "typescript", "typescriptreact", "vue" },
						init_options = {
							plugins = {
								{
									name = "@vue/typescript-plugin",
									location = volar_path,
									languages = { "vue" },
								},
							},
						},
					})
				end,
				-- ["elixirls"] = function()
				-- 	require("lspconfig").elixirls.setup({
				-- 		cmd = { mason_path.bin_prefix() .. "/elixir-ls" },
				-- 	})
				-- end,
				["lexical"] = function()
					require("lspconfig").lexical.setup({
						cmd = { mason_path.bin_prefix() .. "/lexical" },
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
						-- filetypes = { "vue" },
						-- filetypes = { "typescript", "javascript", "javascriptreact", "typescriptreact", "vue", "json" },
						-- init_options = {
						-- 	vue = {
						-- 		hybridMode = true,
						-- 	},
						-- },
						-- capabilities = {
						-- 	workspace = {
						-- 		didChangeWatchedFiles = {
						-- 			dynamicRegistration = true,
						-- 		},
						-- 	},
						-- },
					})
				end,
				["texlab"] = function()
					require("lspconfig").texlab.setup({
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
					-- "elixirls",
					"lexical",
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
		end,
	},
}
