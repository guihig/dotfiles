vim.api.nvim_exec([[
  let bufferline = get(g:, 'bufferline', {})
  let bufferline.animation = v:false
  let bufferline.tabpages = v:false
  let bufferline.closable = v:false
]], true)

