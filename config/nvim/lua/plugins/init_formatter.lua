local keymap = vim.keymap

local util = require("formatter.util")

local mason_pkgs_path = vim.fn.stdpath("data") .. "/mason/packages"

local function js_fmt()
    return {
        exe = mason_pkgs_path
            .. "/prettier/node_modules/prettier/bin/prettier.cjs",
        args = {
            "--stdin-filepath",
            util.escape_path(util.get_current_buffer_file_path())
        },
        stdin = true,
        try_node_modules = true
    }
end

local function py_fmt()
    return {
        exe = mason_pkgs_path .. "/black/venv/bin/black",
        args = { "-q", "-" },
        stdin = true
    }
end

local function ex_fmt()
    return { exe = "mix", args = { "format", "-" }, stdin = true }
end

local function sql_fmt()
    return {
        exe = mason_pkgs_path
            .. "/sql-formatter/node_modules/sql-formatter/bin/sql-formatter-cli.cjs",
        args = { "-l", "postgresql" },
        stdin = true
    }
end

local function lua_fmt()
    return {
        exe = mason_pkgs_path .. "/luaformatter/bin/lua-format",
        args = { "-i", "-c", "~/.config/nvim/assets/lua-format.yaml" },
        stdin = true
    }
end

local function vhdl_fmt()
    return {
        exe = "vhdlformatter.sh",
        args = { util.escape_path(util.get_current_buffer_file_name()) },
        stdin = true
    }
end

local function haskell_fmt() return { exe = "hindent", stdin = true } end

local function latex_fmt()
    return {
        exe = mason_pkgs_path .. "/latexindent/latexindent-linux",
        args = { "-g", "/dev/null" },
        stdin = true
    }
end

local function asm_fmt()
    return {
        exe = "asmfmt",
        args = { vim.api.nvim_buf_get_name(0) },
        stdin = true
    }
end

local function c_fmt()
    return {
        exe = mason_pkgs_path .. "/clang-format/venv/bin/clang-format",
        args = {
            "-assume-filename",
            util.escape_path(util.get_current_buffer_file_name())
        },
        stdin = true,
        try_node_modules = true
    }
end

local function yaml_fmt()
    return {
        exe = mason_pkgs_path .. "/yamlfmt/yamlfmt",
        args = {
            "--stdin-filepath",
            util.escape_path(util.get_current_buffer_file_path()),
            "--parser",
            "yaml"
        },
        stdin = true,
        try_node_modules = true
    }
end

local function rust_fmt() return { exe = "rustfmt", stdin = true } end

require("formatter").setup({
    logging = true,
    filetype = {
        lua = { lua_fmt },
        javascript = { js_fmt },
        javascriptreact = { js_fmt },
        typescript = { js_fmt },
        typescriptreact = { js_fmt },
        vue = { js_fmt },
        html = { js_fmt },
        json = { js_fmt },
        css = { js_fmt },
        scss = { js_fmt },
        sass = { js_fmt },
        sql = { sql_fmt },
        python = { py_fmt },
        elixir = { ex_fmt },
        haskell = { haskell_fmt },
        vhdl = { vhdl_fmt },
        tex = { latex_fmt },
        asm = { asm_fmt },
        cpp = { c_fmt },
        yaml = { yaml_fmt },
        rust = { rust_fmt }
    }
})

local opts = { noremap = true }
keymap.set("n", "<leader>f", "<cmd>FormatWrite<CR>", opts)
