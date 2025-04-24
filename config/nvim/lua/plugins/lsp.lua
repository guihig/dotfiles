local keymap = vim.keymap
local lsp_location = require("lsp_location")

return {
	{
		"folke/lazydev.nvim",
		ft = "lua",
		cmd = "LazyDev",
		opts = {
			library = {
				lsp_location["awesomewm_lib"],
				"~/.config/awesome/modules/rubato",
				"~/.config/awesome/modules/bling",
				"~/.config/awesome/modules/color",
				"~/.config/awesome/lain",
				{ path = "luvit-meta/library", words = { "vim%.uv" } },
				{ path = "LazyVim", words = { "LazyVim" } },
				{ path = "lazy.nvim", words = { "LazyVim" } },
			},
		},
	},
	{
		"saghen/blink.cmp",
		dependencies = {
			{ "rafamadriz/friendly-snippets" },
			{ "Kaiser-Yang/blink-cmp-avante" },
		},
		version = "1.*",
		opts = {
			enabled = function()
				return not vim.tbl_contains({
					"clap_input",
					"guihua",
					"guihia_rust",
					"sagadiagnostic",
					"sagafinder",
					"sagarename",
					"diff",
					"saga_unitest",
				}, vim.bo.filetype) and vim.bo.buftype ~= "prompt" and vim.b.completion ~= false
			end,
			keymap = {
				preset = "none",
				["<C-space>"] = { "show", "show_documentation", "hide_documentation" },
				["<C-e>"] = { "hide" },
				["<CR>"] = { "select_and_accept", "fallback" },

				["<Up>"] = { "select_prev", "fallback" },
				["<Down>"] = { "select_next", "fallback" },
				["<C-p>"] = { "select_prev", "fallback_to_mappings" },
				["<C-n>"] = { "select_next", "fallback_to_mappings" },

				["<C-b>"] = { "scroll_documentation_up", "fallback" },
				["<C-f>"] = { "scroll_documentation_down", "fallback" },

				["<C-K>"] = {
					function(cmp)
						cmp.show({ providers = { "snippets" } })
					end,
				},

				["<Tab>"] = { "snippet_forward", "fallback" },
				["<S-Tab>"] = { "snippet_backward", "fallback" },

				["<C-k>"] = { "show_signature", "hide_signature", "fallback" },
			},
			signature = { enabled = true },
			-- appearance = {
			-- 	nerd_font_variant = "mono",
			-- 	appearance = {
			-- 		-- Blink does not expose its default kind icons so you must copy them all (or set your custom ones) and add Copilot
			-- 		kind_icons = {
			-- 			Copilot = "",
			-- 			Text = "󰉿",
			-- 			Method = "󰊕",
			-- 			Function = "󰊕",
			-- 			Constructor = "󰒓",
			--
			-- 			Field = "󰜢",
			-- 			Variable = "󰆦",
			-- 			Property = "󰖷",
			--
			-- 			Class = "󱡠",
			-- 			Interface = "󱡠",
			-- 			Struct = "󱡠",
			-- 			Module = "󰅩",
			--
			-- 			Unit = "󰪚",
			-- 			Value = "󰦨",
			-- 			Enum = "󰦨",
			-- 			EnumMember = "󰦨",
			--
			-- 			Keyword = "󰻾",
			-- 			Constant = "󰏿",
			--
			-- 			Snippet = "󱄽",
			-- 			Color = "󰏘",
			-- 			File = "󰈔",
			-- 			Reference = "󰬲",
			-- 			Folder = "󰉋",
			-- 			Event = "󱐋",
			-- 			Operator = "󰪚",
			-- 			TypeParameter = "󰬛",
			-- 		},
			-- 	},
			-- },
			completion = {
				documentation = { auto_show = true },
				menu = {
					draw = {
						columns = {
							{ "label", "label_description", gap = 1 },
							{ "kind_icon", "source_name" },
						},
					},
				},
			},
			snippets = { preset = "default" },
			sources = {
				default = { "avante", "lsp", "path", "snippets", "buffer" },
			},
			fuzzy = { implementation = "prefer_rust_with_warning" },
		},
		opts_extend = { "sources.default" },
	},
	{
		"nvimdev/lspsaga.nvim",
		-- dir = "~/dev/nvim/lspsaga.nvim/",
		-- dev = true,
		opts = {
			symbol_in_winbar = { folder_level = 3 },
			code_action = { show_server_name = true, keys = { quit = "<ESC>" } },
			definition = { keys = { quit = "<ESC>" } },
			diagnostic = { max_height = 0.8, keys = { quit = "<ESC>" } },
			rename = { keys = { quit = "<ESC>" } },
			lightbulb = {
				sign = false,
				virtual_text = false,
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
					local keymap_opts = { buffer = bufnr, remap = false }

					-- vim.opt_local.omnifunc = "v:lua.vim.lsp.omnifunc"
					vim.bo[args.buf].formatexpr = nil
					vim.bo[args.buf].omnifunc = nil
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
