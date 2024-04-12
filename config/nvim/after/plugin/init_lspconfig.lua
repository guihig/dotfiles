local keymap = vim.keymap
local lspconfig = require("lspconfig")
local lsp_zero = require("lsp-zero")
local mason_path = require("mason-core.path")

local vue_language_server_path = mason_path.package_prefix() .. "/vue-language-server/node_modules/@vue/language-server"

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
	keymap.set("n", "[e", "<cmd>Lspsaga diagnostic_jump_next<cr>", opts)
	keymap.set("n", "]e", "<cmd>Lspsaga diagnostic_jump_prev<cr>", opts)
	keymap.set("n", "<leader>rn", "<cmd>Lspsaga rename<cr>", opts)
	keymap.set("n", "<leader>o", "<cmd>Lspsaga outline<cr>", opts)
end)

local handlers = {
	lsp_zero.default_setup,
	["tsserver"] = function()
		require("lspconfig").tsserver.setup({
			init_options = {
				plugins = {
					{
						name = "@vue/typescript-plugin",
						location = vue_language_server_path,
						languages = { "vue" },
					},
				},
			},
			filetypes = { "typescript", "javascript", "javascriptreact", "typescriptreact" },
		})
	end,
	["elixirls"] = function()
		require("lspconfig").elixirls.setup({
			cmd = { mason_path.bin_prefix() .. "/elixir-ls" },
		})
	end,
	["jsonls"] = function()
		require("lspconfig").jsonls.setup({
			filetypes = { "json" },
		})
	end,
	["volar"] = function()
		require("lspconfig").volar.setup({
			init_options = {
				vue = {
					hybridMode = false,
				},
				typescript = {
					tsdk = vim.fn.getcwd() .. "node_modules/typescript/lib",
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
		"elixirls",
		"jsonls",
		"tsserver",
		"eslint",
		"cssls",
		"pyright",
		"sqlls",
		"volar",
		"yamlls",
		"rust_analyzer",
		"rnix",
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
