require("neotest").setup({
    adapters = {
        require("neotest-python")({ dap = { justMyCode = false } }),
        require("neotest-plenary"),
        require("neotest-elixir"),
        require("neotest-vim-test")({
            ignore_file_types = { "python", "vim", "lua" }
        })
    }
})

local opts = { noremap = true, silent = true }

vim.keymap.set("n", "<leader>tn", require("neotest").run.run, opts)
vim.keymap.set("n", "<leader>to",
               function() require("neotest").output.open({ enter = true }) end,
               opts)
vim.keymap.set("n", "<leader>tf",
               function() require("neotest").run.run(vim.fn.expand("%")) end,
               opts)
vim.keymap.set("n", "<leader>td",
               function() require("neotest").run.run({ strategy = "dap" }) end,
               opts)
