local cmp = require 'cmp'
local lspkind = require('lspkind')

require("cmp_git").setup {}

-- https://www.youtube.com/watch?v=_DnmphIwnjo&t=1514s
-- https://github.com/tjdevries/config_manager/blob/master/xdg_config/nvim/after/plugin/completion.lua
cmp.setup({
    snippet = {expand = function(args) vim.fn["vsnip#anonymous"](args.body) end},
    -- completion = {completeopt = 'menu,menuone,noinsert'},
    mapping = {
        ['<C-d>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), {'i', 'c'}),
        ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), {'i', 'c'}),
        ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), {'i', 'c'}),
        ['<C-y>'] = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
        ['<C-e>'] = cmp.mapping({
            i = cmp.mapping.abort(),
            c = cmp.mapping.close()
        }),
        ['<CR>'] = cmp.mapping.confirm({
            behavior = cmp.ConfirmBehavior.Insert,
            select = false
        }) -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    },
    sources = cmp.config.sources({
        {name = 'nvim_lua'}, {
            name = 'nvim_lsp'
            -- , max_item_count = 15
        }, {name = 'vsnip'}, {name = "cmp_git"}
    }, {{name = "path"}, {name = 'emoji'}, {name = "buffer"}}),
    formatting = {
        format = lspkind.cmp_format({
            with_text = true,
            menu = ({
                buffer = "[Buffer]",
                nvim_lsp = "[LSP]",
                luasnip = "[LuaSnip]",
                nvim_lua = "[Lua]",
                latex_symbols = "[Latex]"
            })
        })
    },
    completion = {keyword_length = 3},
    experimental = {native_menu = false, ghost_text = true}
})
