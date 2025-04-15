local keymap = vim.keymap

local function tab_name(tab)
	return string.gsub(tab, "%[..%]", "")
end

local function tab_modified(tab)
	local wins = require("tabby.module.api").get_tab_wins(tab)
	for i, x in pairs(wins) do
		if vim.bo[vim.api.nvim_win_get_buf(x)].modified then
			return "󰐕"
		end
	end
	return ""
end

local function buf_modified(buf)
	if vim.bo[buf].modified then
		return "󰐕"
	else
		return ""
	end
end

local function lsp_diagnostics(buf)
	local diagnostics = vim.diagnostic.get(buf)

	local count = { 0, 0, 0, 0 }
	for _, diagnostic in ipairs(diagnostics) do
		count[diagnostic.severity] = count[diagnostic.severity] + 1
	end

	if count[1] > 0 then
		return "󰅚"
	elseif count[2] > 0 then
		return "󰀪"
	end

	return ""
end

local theme = {
	fill = "TabLineFill",
	current_tab = "TSWarning",
	inactive_tab = "TabLineSel",
	head = "TabLine",
	tab = "TabLine",
	win = "TabLine",
	tail = "TabLine",
	left_win_separator = "",
	right_win_separator = "",
	left_tab_separator = "",
	right_tab_separator = "",
}

return {
	"nanozuki/tabby.nvim",
	dependencies = { "nvim-tree/nvim-web-devicons" },
	config = function()
		vim.o.showtabline = 2
		local buf_name = require("tabby.feature.buf_name")

		require("tabby").setup({
			line = function(line)
				return {
					line.bufs().foreach(function(buf)
						local hl = buf.is_current() and theme.current_tab or theme.inactive_tab
						return {
							" ",
							{
								line.sep(theme.left_win_separator, hl, theme.fill),
								buf.file_icon(),
								buf_name.get_by_bufid(buf.id),
								lsp_diagnostics(buf.id),
								buf_modified(buf.id),
								line.sep(theme.right_win_separator, hl, theme.fill),
								hl = hl,
								margin = " ",
							},
						}
					end),

					-- line.wins().foreach(function(win)
					-- 	local hl = win.is_current() and theme.current_tab or theme.inactive_tab
					-- 	return {
					-- 		" ",
					-- 		{
					-- 			line.sep(theme.left_win_separator, hl, theme.fill),
					-- 			win.file_icon(),
					-- 			win_name.get(win.id),
					-- 			lsp_diagnostics(win.buf().id),
					-- 			buf_modified(win.buf().id),
					-- 			line.sep(theme.right_win_separator, hl, theme.fill),
					-- 			hl = hl,
					-- 			margin = " ",
					-- 		},
					-- 	}
					-- end),

					line.spacer(),

					line.tabs().foreach(function(tab, i, total)
						if total < 2 then
							return {}
						end

						local hl = tab.is_current() and theme.current_tab or theme.inactive_tab
						return {
							" ",
							{
								line.sep(theme.left_tab_separator, hl, theme.fill),
								tab.number(),
								tab_name(tab.name()),
								tab_modified(tab.id),
								line.sep(theme.right_tab_separator, hl, theme.fill),
								hl = hl,
								margin = " ",
							},
						}
					end),
					hl = theme.fill,
				}
			end,
		})

		local keymap_opts = { noremap = true, silent = true }
		keymap.set("n", "<S-A-h>", ":bprevious<cr>", keymap_opts)
		keymap.set("n", "<S-A-l>", ":bnext<cr>", keymap_opts)
	end,
}
