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
		"dmmulroy/ts-error-translator.nvim",
		config = true,
		ft = { "typescript", "typescriptreact", "javascript", "javascriptreact", "vue" },
	},
	{ "JoosepAlviste/nvim-ts-context-commentstring" },
}
