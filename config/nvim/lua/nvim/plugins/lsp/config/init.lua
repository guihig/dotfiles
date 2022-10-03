local M = {}

local opts = { noremap = true, silent = true }

local function t(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end

FLOAT_WINID = nil

local function close_float()
    if FLOAT_WINID ~= nil then
        vim.api.nvim_win_close(FLOAT_WINID, false)
        FLOAT_WINID = nil
        return
    else
        return t "<Esc>"
    end
end

local function open_float()
    local _, winid = vim.diagnostic.open_float()
    FLOAT_WINID = winid
end

vim.keymap.set("n", "<Esc>", close_float, opts)
vim.keymap.set("n", "<leader>e", open_float, opts)
vim.keymap.set("n", "[g", vim.diagnostic.goto_prev, opts)
vim.keymap.set("n", "]g", vim.diagnostic.goto_next, opts)

function _G.lsp_organize_imports()
    local context = { only = { "source.organizeImports" } }
    vim.lsp.buf.code_action(context)
end

M.on_attach = function(_, bufnr)
    local l_opts = { noremap = true, silent = true }
    Option.b(bufnr, { omnifunc = "v:lua.vim.lsp.omnifunc" })
    Keybind.b({
        { bufnr, "n", "gD", "<Cmd>lua vim.lsp.buf.declaration()<CR>", l_opts },
        { bufnr, "n", "gd", "<Cmd>lua vim.lsp.buf.definition()<CR>", l_opts },
        { bufnr, "n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", l_opts },
        { bufnr, "n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", l_opts },
        {
            bufnr,
            "n",
            "<leader>D",
            "<cmd>lua vim.lsp.buf.type_definition()<CR>",
            l_opts
        }, -- LspSaga
        { bufnr, "n", "K", "<cmd>Lspsaga hover_doc<CR>", l_opts },
        { bufnr, "n", "<leader>o", "<cmd>LSoutlineToggle<CR>", l_opts },
        { bufnr, "n", "<leader>a", "<cmd>Lspsaga code_action<CR>", l_opts },
        { bufnr, "n", "<leader>rn", "<cmd>Lspsaga rename<CR>", l_opts },
        { bufnr, "n", "<leader>qq", "<cmd>Lspsaga lsp_finder<CR>", l_opts },
        { bufnr, "n", "<leader>k", "<cmd>Lspsaga preview_definition<CR>", l_opts },
        { bufnr, "n", "<C-A-o>", "<cmd>lua lsp_organize_imports()<CR>", l_opts }
    })
end

return M
