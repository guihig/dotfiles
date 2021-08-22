local opt = { noremap = true, expr = true, silent = true, nowait = true }
local silent_opt = { silent = true }
local nrsilent_opt = { silent = true, noremap = true }

Keybind.g({
    { 'i', '<Tab>', [[ pumvisible() ? "\<C-n>" : "\<TAB>" ]], opt },
	{ 'i', '<S-Tab>', '<cmd>lua Coc.go_to_previous_completion_item("<c-h>")<CR>', nrsilent_opt },
	{ 'i', '<CR>', [[pumvisible() ? "\<C-Y>" : "\<CR>"]], opt},

	-- code jumps
	{ 'n', 'gd', '<Plug>(coc-definition)', silent_opt },
	{ 'n', 'gi', '<Plug>(coc-implementation)', silent_opt },
	{ 'n', 'gr', '<Plug>(coc-references)', silent_opt },
	{ 'n', 'gt', '<Plug>(coc-type-definition)', silent_opt },
	{ 'n', 'K', '<cmd>lua Coc.show_documentation()<cr>', silent_opt },

	-- error jumps
	{ 'n', ']g', '<Plug>(coc-diagnostic-next)', silent_opt },
	{ 'n', '[g', '<Plug>(coc-diagnostic-prev)', silent_opt },

	-- actions
	{ 'n', '<leader>rn', '<Plug>(coc-rename)', silent_opt},
	{ 'n', '<leader>a', '<Plug>(coc-codeaction)', silent_opt},
	-- { 'n', '<leader>s', '<Plug>(coc-codeaction-selected)', silent_opt},
	{ 'n', '<leader>qf', '<Plug>(coc-fix-current)', silent_opt},
	{ 'n', '<C-A-o>', '<cmd>lua Coc.organize_import()<CR> :update<CR>', silent_opt},

	-- @TODO there is an error when <c-j> keymap is registered
	-- need to check this
	-- pop item selection
	-- { 'i', '<c-k>', '<cmd>lua Coc.go_to_previous_completion_item("<c-k>")<cr>', silent_opt },
	-- { 'i', [[<c-j>]], '<cmd>lua Coc.go_to_previous_completion_item("<c-j>")<cr>', silent_opt },
	{ 'i', '<c-space>', [[coc#refresh()]], opt },
})
