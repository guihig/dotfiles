local keymap = vim.keymap
local lspconfig = require("lspconfig")
local utils = require("utils")
local capabilities = require("cmp_nvim_lsp").default_capabilities()

-- Helpers
-- FLOAT_WINID = nil
-- local function close_diag_float()
--     if FLOAT_WINID ~= nil then
--         pcall(vim.api.nvim_win_close, FLOAT_WINID, true)
--         FLOAT_WINID = nil
--         return
--     else
--         return utils.t "<Esc>"
--     end
-- end
--
-- local function open_diag_float()
--     local _, winid = vim.diagnostic.open_float()
--     FLOAT_WINID = winid
-- end

-- local keymap_opts = { noremap = true, silent = true }
-- keymap.set("n", "<Esc>", close_diag_float, keymap_opts)
-- keymap.set("n", "<leader>e", open_diag_float, keymap_opts)
-- keymap.set("n", "[g", vim.diagnostic.goto_prev, keymap_opts)
-- keymap.set("n", "]g", vim.diagnostic.goto_next, keymap_opts)

vim.api.nvim_create_autocmd("LspAttach", {
	group = vim.api.nvim_create_augroup("UserLspConfig", {}),
	callback = function(ev)
		-- Enable completion triggered by <c-x><c-o>
		vim.bo[ev.buf].omnifunc = "v:lua.vim.lsp.omnifunc"

		-- Buffer local mappings.
		local opts = { buffer = ev.buf }

		keymap.set("n", "gd", "<cmd>Lspsaga goto_definition<cr>", opts)
		keymap.set("n", "gr", "<cmd>Lspsaga finder<cr>", opts)
		keymap.set("n", "gi", "<cmd>Lspsaga peek_definition<cr>", opts)
		keymap.set("n", "gs", "<cmd>Lspsaga peek_type_definition<cr>", opts)
		keymap.set({ "n", "v" }, "<leader>a", "<cmd>Lspsaga code_action<cr>", opts)
		keymap.set("n", "K", "<cmd>Lspsaga hover_doc<cr>", opts)
		keymap.set("n", "<leader>e", "<cmd>Lspsaga show_cursor_diagnostics<cr>", opts)
		keymap.set("n", "[e", "<cmd>Lspsaga diagnostic_jump_next<cr>", opts)
		keymap.set("n", "]e", "<cmd>Lspsaga diagnostic_jump_prev<cr>", opts)
		keymap.set("n", "<leader>rn", "<cmd>Lspsaga rename<cr>", opts)
		keymap.set("n", "<leader>o", "<cmd>Lspsaga outline<cr>", opts)

		-- vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts)
		-- vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
		-- vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
		-- vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opts)
		-- vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, opts)
		-- vim.keymap.set("n", "<space>D", vim.lsp.buf.type_definition, opts)
		-- vim.keymap.set("n", "<space>rn", vim.lsp.buf.rename, opts)
		-- -- vim.keymap.set({ "n", "v" }, "<space>a", vim.lsp.buf.code_action, opts)
		-- vim.keymap.set({ "n", "v" }, "<space>a", ":LspUI code_action<cr>", opts)
		-- vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
	end,
})

-- Init
vim.cmd([[highlight NormalFloat guibg=none]])
vim.cmd([[highlight FloatBorder guifg=white guibg=none]])
-- vim.lsp.set_log_level("debug")

-- To instead override globally
-- local orig_util_open_floating_preview = vim.lsp.util.open_floating_preview
-- function vim.lsp.util.open_floating_preview(contents, syntax, opts, ...)
--     opts = opts or {}
--     opts.border = opts.border or "rounded"
--     return orig_util_open_floating_preview(contents, syntax, opts, ...)
-- end

lspconfig.util.default_config = vim.tbl_extend("force", lspconfig.util.default_config, { autostart = true })

-- Servers cfgs
local servers = {
	elixirls = {},
	cssls = {},
	tailwindcss = {},
	hls = {},
	jsonls = { filetypes = { "json" } },
	lua_ls = {
		settings = {
			Lua = {
				completion = { autoRequire = true },
				runtime = {
					-- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
					version = "LuaJIT",
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
						"mouse",
					},
					unusedLocalExclude = { "_%s" },
				},
				workspace = {
					-- Make the server aware of Neovim runtime files
					library = vim.api.nvim_get_runtime_file("", true),
				},
				-- Do not send telemetry data containing a randomized but unique identifier
				telemetry = { enable = false },
			},
		},
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
	pyright = {},
	efm = {
		filetypes = {
			"javascript",
			"javascriptreact",
			"typescript",
			"typescriptreact",
			"vue",
			"elixir",
		},
	},
	volar = {
		filetypes = {
			-- "typescript",
			"javascript",
			"javascriptreact",
			-- "typescriptreact",
			"vue",
		},
	},
	rnix = {},
	yamlls = {},
	rust_analyzer = {},
	clangd = {},
}

for server, server_cfg in pairs(servers) do
	local cfg = { capabilities = capabilities }
	local merged_cfg = utils.merge_table(cfg, server_cfg)
	lspconfig[server].setup(merged_cfg)
end
