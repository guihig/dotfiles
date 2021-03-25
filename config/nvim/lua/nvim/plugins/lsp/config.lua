-- Option.g({
--   omnifunc = 'v:lua.vim.lsp.omnifunc'
-- })

local opts = { noremap=true, silent=true }

-- Variable.g({
--   completion_confirm_key = '',
--   completion_enable_snippet = 'UltiSnips',
--   completion_trigger_character = {'.', '::'},
--   completion_auto_change_source = 1,
--   completion_chain_complete_list = {
--     {['complete_items'] = {'lsp'}},
--     {['complete_items'] = {'snippet'}},
--     {['mode'] = '<c-p>'},
--     {['mode'] = '<c-n>'},
--   }
-- })

Keybind.g({
  {'n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts},
  {'n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts},
  -- {'n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts},
  {'n', 'K', '<cmd>Lspsaga hover_doc<CR>', opts},
  {'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts},
  -- {'n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts},
  {'n', '<C-k>', '<cmd>Lspsaga signature_help<CR>', opts},
  {'n', '<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts},
  {'n', '<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts},
  {'n', '<leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts},
  {'n', '<leader>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts},
  {'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts},
  -- {'n', '<leader>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts},
  {'n', '<leader>e', '<cmd>Lspsaga show_line_diagnostics<CR>', opts},
  {'n', '[g', '<cmd>Lspsaga diagnostic_jump_pre<CR>', opts},
  {'n', ']g', '<cmd>Lspsaga diagnostic_jump_next<CR>', opts},
  {'n', '<leader>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts},
  {'n', '<leader>a', '<cmd>Lspsaga code_action<CR>', opts},
  {'n', '<leader>rn', '<cmd>Lspsaga rename<CR>', opts},
  {'n', '<leader>qq', '<cmd>Lspsaga lsp_finder<CR>', opts},
  {'n', '<leader>f', '<cmd>Format<CR>:update<CR>', opts},
  {'n', '<C-A-o>', '<cmd>OrganizeImports<CR>:update<CR>', opts},

  -- Completion Bindings
  -- {'i', '<Tab>', [[pumvisible() ? "\<C-n>" : "\<Tab>"]], {noremap=true,expr=true}},
  -- {'i', '<S-Tab>', [[pumvisible() ? "\<C-p>" : "\<S-Tab>"]], {noremap=true,expr=true}},
  -- {'i', '<CR>', [[pumvisible() ? complete_info()["selected"] != "-1" ? "\<Plug>(completion_confirm_completion)"  : "\<c-e>\<CR>" :  "\<CR>"]], {expr=true}},
  -- {'i', '<C-space>', '<Plug>(completion_trigger)', {silent=true}}
})

-- vim.api.nvim_exec([[
--   augroup lsp_document_highlight
--     autocmd! * <buffer>
--     autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
--     autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
--   augroup END
-- ]], false)
