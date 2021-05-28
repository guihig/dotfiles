Variable.g({user_emmet_install_global = 0, user_emmet_leader_key = '<C-y>'})

vim.api.nvim_exec([[
  autocmd FileType html,css,vue EmmetInstall
]], true)
