local cmp = require 'cmp'

local has_words_before = function()
    if vim.api.nvim_buf_get_option(0, "buftype") == "prompt" then
        return false
    end
    local line, col = unpack(vim.api.nvim_win_get_cursor(0))
    return col ~= 0 and
               vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col,
                                                                          col)
                   :match("%s") == nil
end

local feedkey = function(key, mode)
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true),
                          mode, true)
end

cmp.setup({
    snippet = {expand = function(args) vim.fn["vsnip#anonymous"](args.body) end},
    completion = {completeopt = 'menu,menuone,noinsert'},
    mapping = {
        ["<Tab>"] = cmp.mapping(function(fallback)
            if vim.fn.pumvisible() == 1 then
                feedkey("<C-n>", "n")
            elseif vim.fn["vsnip#available"]() == 1 then
                feedkey("<Plug>(vsnip-expand-or-jump)", "")
            elseif has_words_before() then
                cmp.complete()
            else
                fallback() -- The fallback function sends a already mapped key. In this case, it's probably `<Tab>`.
            end
        end, {"i", "s"}),

        ["<S-Tab>"] = cmp.mapping(function()
            if vim.fn.pumvisible() == 1 then
                feedkey("<C-p>", "n")
            elseif vim.fn["vsnip#jumpable"](-1) == 1 then
                feedkey("<Plug>(vsnip-jump-prev)", "")
            end
        end, {"i", "s"}),
        ['<C-d>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.close(),
        ['<CR>'] = cmp.mapping.confirm({select = true})
    },
    sources = {
        {name = 'nvim_lsp'}, {name = 'nvim_lua'}, {name = 'path'},
        {name = 'vsnip'}
        -- {name = 'buffer'}
    }
})
