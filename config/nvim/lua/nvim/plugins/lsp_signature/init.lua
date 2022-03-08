local cfg = {
    floating_window = false, -- show hint in a floating window, set to false for virtual text only mode
    fix_pos = true -- set to true, the floating window will not auto-close until finish all parameters
}
require'lsp_signature'.setup(cfg)
