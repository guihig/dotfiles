local keymap = vim.keymap

local M = {}

local function t(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local function lsp_organize_imports()
    local context = { only = { "source.organizeImports" } }
    vim.lsp.buf.code_action(context)
end

FLOAT_WINID = nil
local function close_diag_float()
    if FLOAT_WINID ~= nil then
        vim.api.nvim_win_close(FLOAT_WINID, false)
        FLOAT_WINID = nil
        return
    else
        return t "<Esc>"
    end
end

local function open_diag_float()
    local _, winid = vim.diagnostic.open_float()
    FLOAT_WINID = winid
end

local opts = { noremap = true, silent = true }
keymap.set("n", "<Esc>", close_diag_float, opts)
keymap.set("n", "<leader>e", open_diag_float, opts)
keymap.set("n", "[g", vim.diagnostic.goto_prev, opts)
keymap.set("n", "]g", vim.diagnostic.goto_next, opts)

M.on_attach = function(_, bufnr)
    vim.bo[bufnr].omnifunc = "v:lua.vim.lsp.omnifunc"

    local bufopts = { noremap = true, silent = true, buffer = bufnr }
    keymap.set("n", "gD", vim.lsp.buf.declaration, bufopts)
    keymap.set("n", "gd", vim.lsp.buf.definition, bufopts)
    keymap.set("n", "gi", vim.lsp.buf.implementation, bufopts)
    keymap.set("n", "gr", vim.lsp.buf.references, bufopts)
    keymap.set("n", "K", vim.lsp.buf.hover, bufopts)
    keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, bufopts)
    keymap.set("n", "<leader>D", vim.lsp.buf.type_definition, bufopts)
    keymap.set("n", "K", vim.lsp.buf.hover, bufopts)
    keymap.set("n", "<leader>a", vim.lsp.buf.code_action, bufopts)
    keymap.set("n", "<leader>rn", vim.lsp.buf.rename, bufopts)
    keymap.set("n", "<C-A-o>", lsp_organize_imports, bufopts)

end

return M
