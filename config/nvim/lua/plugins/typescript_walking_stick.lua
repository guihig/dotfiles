local keymap = vim.keymap
local lsp_location = require("lsp_location")

return {
	{
		"dmmulroy/tsc.nvim",
		config = function()
			require("tsc").setup({
				bin_path = "vue-tsc",
			})
		end,
	},
	{ "catgoose/vue-goto-definition.nvim" },
	{
		"youyoumu/pretty-ts-errors.nvim",
	},
	{ "dmmulroy/ts-error-translator.nvim" },
	{
		"windwp/nvim-ts-autotag",
		config = function(_, _)
			require("nvim-ts-autotag").setup()
		end,
	},
	{
		"JoosepAlviste/nvim-ts-context-commentstring",
		config = function()
			require("ts_context_commentstring").setup({
				enable_autocmd = false,
			})
			local native_get_option = vim.filetype.get_option
			---@diagnostic disable-next-line: duplicate-set-field
			vim.filetype.get_option = function(filetype, option)
				return option == "commentstring"
						and require("ts_context_commentstring.internal").calculate_commentstring()
					or native_get_option(filetype, option)
			end
		end,
	},
	-- {
	-- 	"pmizio/typescript-tools.nvim",
	-- 	dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
	-- 	opts = {
	-- 		on_attach = function(_, bufnr)
	-- 			local keymap_opts = { noremap = true, buffer = bufnr }
	-- 			keymap.set("n", "<C-A-o>", "<cmd>TSToolsOrganizeImports<CR>", keymap_opts)
	-- 		end,
	-- 		filetypes = { "javascript", "javascriptreact", "typescript", "typescriptreact", "vue" },
	-- 		settings = {
	-- 			expose_as_code_action = { "organize_imports" },
	-- 			tsserver_plugins = {
	-- 				"@vue/typescript-plugin",
	-- 			},
	-- 		},
	-- 	},
	-- },
}
