-- local keymap = vim.keymap
-- require("printer").setup({
-- 	behavior = "insert_below",
-- 	formatters = {
-- 		elixir = function(text_inside, text_var)
-- 			return string.format('IO.inspect(%s, label: "%s")', text_var, text_inside)
-- 		end,
-- 		vue = function(text_inside, text_var)
-- 			return string.format('console.log("%s = ", %s)', text_inside, text_var)
-- 		end,
-- 	},
-- 	add_to_inside = function(text)
-- 		return string.format("[%s:%s] %s", vim.fn.expand("%"), vim.fn.line("."), text)
-- 	end,
-- })
--
-- keymap.set("n", "<C-p>", "<Plug>(printer_below)")
-- keymap.set("v", "<C-p>", "<Plug>(printer_below)")

vim.g.vim_printer_print_below_keybinding = "<C-p>"
vim.g.vim_printer_print_above_keybinding = "<C-S-p>"
vim.g.vim_printer_items = {
	elixir = 'IO.inspect({$}, label: "{$}")',
	javascript = 'console.log("{$}:", {$})',
}
