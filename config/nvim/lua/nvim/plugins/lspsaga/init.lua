LspSaga = require("lspsaga")

LspSaga.init_lsp_saga {
    code_action_keys = {quit = '<Esc>', exec = '<CR>'},
    rename_action_keys = {quit = '<Esc>', exec = '<CR>'},
    finder_action_keys = {
        open = 'o',
        vsplit = 'v',
        split = 'x',
        quit = '<Esc>',
        scroll_down = '<Tab>',
        scroll_up = '<S-Tab>' -- quit can be a table
    }
}
