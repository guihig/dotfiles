-- local util = require("formatter.util")

local function prettier()
	return {
		exe = "prettier",
		args = { "--stdin-filepath", vim.api.nvim_buf_get_name(0) },
		stdin = true,
	}
end

local function black()
	return { exe = "black", args = { "-q", "-" }, stdin = true }
end

local function mix_format()
	return { exe = "mix", args = { "format", "mix.exs", "-" }, stdin = true }
end

local function luaformat()
	return {
		exe = "lua-format",
		args = { "-i", "-c", "~/.config/nvim/lua/nvim/plugins/formatter/lua-format.yaml" },
		stdin = true,
	}
end

local function vhdlFormatter()
	return {
		exe = "vhdlformatter.sh",
		args = { vim.api.nvim_buf_get_name(0) },
		stdin = true,
	}
end

local function hindent()
	return { exe = "hindent", stdin = true }
end

local function latexindent()
	return {
		exe = "latexindent",
		args = { "-d", vim.api.nvim_buf_get_name(0) },
		stdin = true,
	}
end

local function asmfmt()
	return { exe = "asmfmt", args = { vim.api.nvim_buf_get_name(0) }, stdin = true }
end

local function clang_format()
	return {
		exe = "clang-format",
		args = { vim.api.nvim_buf_get_name(0) },
		stdin = true,
	}
end

require("formatter").setup({
	logging = true,
	filetype = {
		-- lua = {
		-- 	require("formatter.filetypes.lua").stylua,
		-- 	function()
		-- 		-- Supports conditional formatting
		-- 		if util.get_current_buffer_file_name() == "special.lua" then
		-- 			return nil
		-- 		end
		--
		-- 		-- Full specification of configurations is down below and in Vim help
		-- 		-- files
		-- 		return {
		-- 			exe = "stylua",
		-- 			args = {
		-- 				"--search-parent-directories",
		-- 				"--stdin-filepath",
		-- 				util.escape_path(util.get_current_buffer_file_path()),
		-- 				"--",
		-- 				"-",
		-- 			},
		-- 			stdin = true,
		-- 		}
		-- 	end,
		-- },
		lua = { luaformat },
		javascript = { prettier },
		javascriptreact = { prettier },
		typescript = { prettier },
		typescriptreact = { prettier },
		vue = { prettier },
		html = { prettier },
		json = { prettier },
		css = { prettier },
		scss = { prettier },
		sass = { prettier },
		python = { black },
		elixir = { mix_format },
		haskell = { hindent },
		vhdl = { vhdlFormatter },
		tex = { latexindent },
		asm = { asmfmt },
		cpp = { clang_format },
	},
})
