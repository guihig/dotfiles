Variable.g({
  completion_confirm_key = '',
  completion_enable_snippet = 'UltiSnips',
  completion_trigger_character = {'.', '::'},
  completion_auto_change_source = 1,
  completion_chain_complete_list = {
    {['complete_items'] = {'lsp'}},
    {['complete_items'] = {'snippet'}},
    {['mode'] = '<c-p>'},
    {['mode'] = '<c-n>'},
  }
})

Keybind.g({
  -- Completion Bindings
  {'i', '<Tab>', [[pumvisible() ? "\<C-n>" : "\<Tab>"]], {noremap=true,expr=true}},
  {'i', '<S-Tab>', [[pumvisible() ? "\<C-p>" : "\<S-Tab>"]], {noremap=true,expr=true}},
  {'i', '<CR>', [[pumvisible() ? complete_info()["selected"] != "-1" ? "\<Plug>(completion_confirm_completion)"  : "\<c-e>\<CR>" :  "\<CR>"]], {expr=true}},
  {'i', '<C-space>', '<Plug>(completion_trigger)', {silent=true}}
})
