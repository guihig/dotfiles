vim.api.nvim_exec([[
  autocmd FileType elixir setlocal ts=2 sts=2 sw=2
  autocmd FileType javascript setlocal ts=2 sts=2 sw=2
  autocmd FileType typescript setlocal ts=2 sts=2 sw=2
  autocmd FileType vue setlocal ts=2 sts=2 sw=2
]], true)
