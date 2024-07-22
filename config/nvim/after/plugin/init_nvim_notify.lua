require("notify").setup({
	background_colour = "#000000",
	timeout = 1000,
	render = "compact",
})

vim.notify = require("notify")
