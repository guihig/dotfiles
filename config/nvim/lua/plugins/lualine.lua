local function get_active_lsp()
	local msg = "[NO LSP]"
	local buf_ft = vim.api.nvim_get_option_value("filetype", {
		buf = 0,
	})
	local clients = vim.lsp.get_clients()
	if next(clients) == nil then
		return msg
	end
	for _, client in ipairs(clients) do
		---@diagnostic disable-next-line: undefined-field
		local filetypes = client.config.filetypes
		if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
			return client.name
		end
	end
	return msg
end

local MODE_MAP = {
	["NORMAL"] = "N",
	["O-PENDING"] = "N?",
	["INSERT"] = "I",
	["VISUAL"] = "V",
	["V-BLOCK"] = "VB",
	["V-LINE"] = "VL",
	["V-REPLACE"] = "VR",
	["REPLACE"] = "R",
	["COMMAND"] = "!",
	["SHELL"] = "SH",
	["TERMINAL"] = "T",
	["EX"] = "X",
	["S-BLOCK"] = "SB",
	["S-LINE"] = "SL",
	["SELECT"] = "S",
	["CONFIRM"] = "Y?",
	["MORE"] = "M",
}

return {
	"hoob3rt/lualine.nvim",
	opts = function()
		return {
			options = {
				icons_enabled = true,
				theme = "gruvbox-material",
				component_separators = { left = "", right = "" },
				section_separators = { left = "", right = "" },
				disabled_filetypes = {},
				always_divide_middle = true,
				globalstatus = false,
			},
			sections = {
				lualine_a = {
					{
						"mode",
						fmt = function(s)
							return MODE_MAP[s] or s
						end,
						color = { gui = "bold" },
					},
				},
				lualine_b = {
					"branch",
					"diff",
					"diagnostics",
					{
						get_active_lsp,
						color = { fg = "#ffffff", gui = "bold" },
					},
				},
				lualine_c = { { "filetype", icon_only = true, separator = "" }, "filename" },
				lualine_x = {
					"encoding",
					"fileformat",
				},
				lualine_y = {},
				lualine_z = { "location" },
			},
			inactive_sections = {
				lualine_a = {},
				lualine_b = {},
				lualine_c = { { "filetype", icon_only = true, separator = "" }, "filename" },
				lualine_x = {},
				lualine_y = {},
				lualine_z = {},
			},
			tabline = {},
			extensions = {},
		}
	end,
	config = function(_, opts)
		require("lualine").setup(opts)
	end,
}
