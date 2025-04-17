return {
	"windwp/nvim-autopairs",
	event = "InsertEnter",
	opts = {
		disable_filetype = {
			"TelescopePrompt",
			"vim",
			"guihua",
			"guihua_rust",
			"clap_input",
		},
		disable_in_macro = true,
	},
	config = function(_, opts)
		local npairs = require("nvim-autopairs")
		npairs.setup(opts)
		npairs.add_rules(require("nvim-autopairs.rules.endwise-elixir"))
		npairs.add_rules(require("nvim-autopairs.rules.endwise-lua"))
		npairs.add_rules(require("nvim-autopairs.rules.endwise-ruby"))
	end,
}
