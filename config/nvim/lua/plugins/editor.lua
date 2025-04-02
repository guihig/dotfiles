local keymap = vim.keymap

function highlight_unlabeled_phase_one_targets(targets, first_idx, last_idx)
	local hl = require("leap.highlight")
	for i = first_idx or 1, last_idx or #targets do
		local target = targets[i]
		if not target.label and target.chars then
			local bufnr = target.wininfo.bufnr
			local id = vim.api.nvim_buf_set_extmark(bufnr, hl.ns, target.pos[1] - 1, target.pos[2] - 1, {
				virt_text = { { table.concat(target.chars), "LeapMatch" } },
				virt_text_pos = "overlay",
				hl_mode = "combine",
				priority = hl.priority.label,
			})
			-- This way Leap automatically cleans up your stuff together with its own.
			table.insert(hl.extmarks, { bufnr, id })
		end
	end
	-- Continue with Leap's native function body.
	return true
end

return {
	{ "tpope/vim-repeat" },
	{ "psliwka/vim-smoothie" },
	{
		"m4xshen/hardtime.nvim",
		dependencies = { "MunifTanjim/nui.nvim", "nvim-lua/plenary.nvim" },
		opts = {
			max_count = 10,
			disable_mouse = false,
			allow_different_key = true,
		},
	},
	{
		"ggandor/leap.nvim",
		config = function()
			vim.api.nvim_set_hl(0, "LeapBackdrop", { link = "Comment" })
			vim.api.nvim_set_hl(0, "LeapMatch", {
				fg = "white",
				bold = true,
				nocombine = true,
			})
			-- vim.api.nvim_set_hl(0, "LeapLabelPrimary", { bg = "green" })
			-- vim.api.nvim_set_hl(0, "LeapLabelSecondary", { bg = "blue" })
			keymap.set({ "n", "x", "o", "v" }, "<leader>s", "<Plug>(leap)")
			keymap.set({ "n", "x", "o", "v" }, "<leader>S", "<Plug>(leap-from-window)")
			-- Hide the (real) cursor when leaping, and restore it afterwards.
			vim.api.nvim_create_autocmd("User", {
				pattern = "LeapEnter",
				callback = function()
					vim.cmd.hi("Cursor", "blend=100")
					vim.opt.guicursor:append({ "a:Cursor/lCursor" })
				end,
			})
			vim.api.nvim_create_autocmd("User", {
				pattern = "LeapLeave",
				callback = function()
					vim.cmd.hi("Cursor", "blend=0")
					vim.opt.guicursor:remove({ "a:Cursor/lCursor" })
				end,
			})
		end,
	},
	{
		"ggandor/flit.nvim",
		config = function()
			require("flit").setup()
		end,
	},
	{
		"chrisgrieser/nvim-early-retirement",
		opts = {
			notificationOnAutoClose = true,
			deleteBufferWhenFileDeleted = true,
		},
		event = "VeryLazy",
	},
	{
		"theKnightsOfRohan/csvlens.nvim",
		dependencies = {
			"akinsho/toggleterm.nvim",
		},
		config = true,
	},
	{
		"NvChad/nvim-colorizer.lua",
		opts = {
			user_default_options = {
				mode = "virtualtext",
			},
		},
	},
}
