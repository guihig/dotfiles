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
}
