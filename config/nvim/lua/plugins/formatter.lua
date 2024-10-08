local keymap = vim.keymap

return {
	"mhartington/formatter.nvim",
	opts = function()
		local lua_fmt = require("formatter.filetypes.lua").stylua
		local js_fmt = require("formatter.filetypes.javascript").prettier
		local ts_fmt = require("formatter.filetypes.typescript").prettier
		local vue_fmt = require("formatter.filetypes.vue").prettier
		local sql_fmt = require("formatter.filetypes.sql").pgformat
		local py_fmt = require("formatter.filetypes.python").black
		local latex_fmt = require("formatter.filetypes.latex").latexindent
		local nix_fmt = require("formatter.filetypes.nix").alejandra
		local yaml_fmt = require("formatter.filetypes.yaml").yamlfmt
		local rust_fmt = require("formatter.filetypes.rust").rustfmt
		local elixir_fmt = require("formatter.filetypes.elixir").mixformat
		local haskell_fmt = require("formatter.filetypes.haskell").stylish_haskell
		return {
			logging = true,
			filetype = {
				lua = { lua_fmt },
				javascript = { js_fmt },
				javascriptreact = { js_fmt },
				typescript = { ts_fmt },
				typescriptreact = { ts_fmt },
				vue = { vue_fmt },
				html = { js_fmt },
				json = { js_fmt },
				rasi = { js_fmt },
				css = { js_fmt },
				scss = { js_fmt },
				sass = { js_fmt },
				sql = { sql_fmt },
				python = { py_fmt },
				elixir = { elixir_fmt },
				haskell = { haskell_fmt },
				tex = { latex_fmt },
				yaml = { yaml_fmt },
				rust = { rust_fmt },
				nix = { nix_fmt },
			},
		}
	end,
	config = function(_, opts)
		require("formatter").setup(opts)
		local keymap_opts = { noremap = true }
		keymap.set("n", "<leader>f", "<cmd>FormatWrite<CR>", keymap_opts)
	end,
}
