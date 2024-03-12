local keymap = vim.keymap

require("printer").setup({
	keymap = "gp",
	behavior = "insert_below",
	formatters = {
		elixir = function(text_inside, text_var)
			return string.format('IO.inspect(%s, label: "%s")', text_var, text_inside)
		end,
		vue = function(text_inside, text_var)
			return string.format('console.log("%s = ", %s)', text_inside, text_var)
		end,
	},
	add_to_inside = function(text)
		return string.format("%s", text)
	end,
})

keymap.set({ "n", "v" }, "<C-p>", "<Plug>(printer_below)")
