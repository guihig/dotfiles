vim.g.vim_printer_print_below_keybinding = "<C-p>"
vim.g.vim_printer_print_above_keybinding = "<C-S-p>"
vim.g.vim_printer_items = {
    elixir = "IO.inspect({$}, label: \"{$}\")",
    javascript = "console.log(\"{$}: \", {$})",
    typescript = "console.log(\"{$}: \", {$})",
    vue = "console.log(\"{$}: \", {$})"
}