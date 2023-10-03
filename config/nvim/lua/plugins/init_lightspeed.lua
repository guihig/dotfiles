local keymap = vim.keymap

keymap.set("n", "<leader>s", "<Plug>Lightspeed_s", {})
keymap.set("n", "<leader>S", "<Plug>Lightspeed_S", {})
keymap.set("v", "<leader>s", "<Plug>Lightspeed_s", {})
keymap.set("v", "<leader>S", "<Plug>Lightspeed_S", {})

require("lightspeed").setup({})
