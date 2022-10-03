local dap, dapui = require("dap"), require("dapui")

local USER = vim.fn.expand("$USER")

dap.adapters.mix_task = {
    type = "executable",
    command = "/home/" .. USER .. "/.lsp/elixir-ls/debugger.sh",
    args = {}
}

dap.configurations.elixir = {
    {
        type = "mix_task",
        name = "mix phx.server",
        task = "phx.server",
        request = "launch",
        projectDir = "${workspaceFolder}",
        excludeModules = { "Qrusty.Native" }
        -- requireFiles = { "test/**/test_helper.exs", "test/**/*_test.exs" }
    }
}

dapui.setup()

dap.listeners.after.event_initialized["dapui_config"] =
    function() dapui.open() end
dap.listeners.before.event_terminated["dapui_config"] =
    function() dapui.close() end
dap.listeners.before.event_exited["dapui_config"] = function() dapui.close() end

dap.defaults.fallback.external_terminal = {
    command = "/usr/bin/alacritty",
    args = { "-e" }
}

local opts = { noremap = true, silent = true }
vim.keymap.set("n", "<leader>b", dap.toggle_breakpoint, opts)
vim.keymap.set("n", "<leader>B", function()
    dap.set_breakpoint(vim.fn.input("Breakpoint condition: "))
end, opts)
vim.keymap.set("n", "<leader>lp", function()
    dap.set_breakpoint(nil, nil, vim.fn.input("Log point message: "))
end, opts)
vim.keymap.set("n", "<leader>dr", dap.repl.open, opts)
vim.keymap.set("n", "<leader>dl", dap.run_last, opts)
vim.keymap.set("n", "<leader>dc", dap.clear_breakpoints, opts)
vim.keymap.set("n", "<F5>", dap.continue, opts)
vim.keymap.set("n", "<F9>", dap.step_over, opts)
vim.keymap.set("n", "<F10>", dap.step_into, opts)
vim.keymap.set("n", "<F11>", dap.step_out, opts)
