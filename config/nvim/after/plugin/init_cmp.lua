local cmp = require("cmp")
local types = require("cmp.types")
local lspkind = require("lspkind")

require("luasnip.loaders.from_vscode").lazy_load()

if vim.o.ft == "clap_input" and vim.o.ft == "guihua" and vim.o.ft == "guihua_rust" then
	cmp.setup.buffer({ completion = { enable = false } })
end

-- https://www.youtube.com/watch?v=_DnmphIwnjo&t=1514s
-- https://github.com/tjdevries/config_manager/blob/master/xdg_config/nvim/after/plugin/completion.lua
cmp.setup({
	snippet = {
		expand = function(args)
			require("luasnip").lsp_expand(args.body)
		end,
	},
	mapping = {
		["<C-d>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
		["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),
		["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
		["<C-y>"] = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
		["<C-e>"] = cmp.mapping({
			i = cmp.mapping.abort(),
			c = cmp.mapping.close(),
		}),
		["<C-n>"] = cmp.mapping(
			cmp.mapping.select_next_item({
				behavior = types.cmp.SelectBehavior.Insert,
			}),
			{ "i", "c" }
		),
		["<C-p>"] = cmp.mapping(
			cmp.mapping.select_prev_item({
				behavior = types.cmp.SelectBehavior.Insert,
			}),
			{ "i", "c" }
		),
		["<CR>"] = cmp.mapping.confirm({
			behavior = cmp.ConfirmBehavior.Insert,
			select = false,
		}), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
	},
	sources = cmp.config.sources({
		{ name = "nvim_lua" },
		{ name = "nvim_lsp" },
		{ name = "luasnip" },
	}, {
		{ name = "async_path" },
		{ name = "buffer", keyword_length = 5 },
	}),
	formatting = {
		format = lspkind.cmp_format({
			with_text = false,
			maxwidth = 50,
			menu = {
				buffer = "[Buf]",
				luasnip = "[Snip]",
			},
		}),
	},
	experimental = { native_menu = false, ghost_text = false },
})