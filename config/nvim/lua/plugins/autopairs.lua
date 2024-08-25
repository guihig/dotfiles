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
	},
	config = function(_, opts)
		local npairs = require("nvim-autopairs")
		npairs.setup(opts)

		local cmp_autopairs = require("nvim-autopairs.completion.cmp")
		local cmp = require("cmp")
		cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done({ map_char = { tex = "" } }))

		npairs.add_rules(require("nvim-autopairs.rules.endwise-elixir"))
		npairs.add_rules(require("nvim-autopairs.rules.endwise-lua"))
		npairs.add_rules(require("nvim-autopairs.rules.endwise-ruby"))
	end,
}
