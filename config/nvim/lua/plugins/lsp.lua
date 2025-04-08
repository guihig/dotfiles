local keymap = vim.keymap
local lsp_location = require("lsp_location")

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
			{ "saghen/blink.cmp" },
			{ "b0o/schemastore.nvim" },
		},
		opts = {
			servers = {
				dockerls = true,
				elixirls = true,
				-- emmet_ls = true,
				erlangls = true,
				eslint = true,
				html = true,
				pyright = true,
				rust_analyzer = true,
				sqlls = true,
				lua_ls = true,
				nil_ls = true,
				volar = true,
				ts_ls = function()
					return {
						filetypes = { "javascript", "javascriptreact", "typescript", "typescriptreact", "vue" },
						init_options = {
							plugins = {
								{
									name = "@vue/typescript-plugin",
									location = lsp_location["vue_ts_plugin"],
									languages = { "javascript", "typescript", "vue" },
								},
							},
						},
					}
				end,
				jsonls = function()
					return {
						settings = {
							json = {
								schemas = require("schemastore").json.schemas(),
								validate = { enable = true },
							},
						},
					}
				end,
				cssls = function()
					local capabilities = vim.lsp.protocol.make_client_capabilities()
					capabilities.textDocument.completion.completionItem.snippetSupport = true
					return {
						capabilities = capabilities,
					}
				end,
				texlab = function()
					return {
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
					}
				end,
			},
		},
		config = function(_, opts)
			local lspconfig = require("lspconfig")

			vim.api.nvim_create_autocmd("LspAttach", {
				callback = function(args)
					local bufnr = args.buf
					local client = assert(vim.lsp.get_client_by_id(args.data.client_id), "must have valid client")

					local keymap_opts = { buffer = bufnr, remap = false }

					vim.opt_local.omnifunc = "v:lua.vim.lsp.omnifunc"
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
				end,
			})

			local function setup_server(lsp_name, server_opts)
				if server_opts == nil then
					server_opts = {}
				end

				server_opts.capabilities = require("blink.cmp").get_lsp_capabilities(server_opts.capabilities)

				if lsp_location[lsp_name] then
					server_opts.cmd = lsp_location[lsp_name]
				end

				lspconfig[lsp_name].setup(server_opts)
			end

			for server, server_opts in pairs(opts.servers) do
				opts = server_opts == true and {} or server_opts()
				setup_server(server, opts)
			end
		end,
	},
}
