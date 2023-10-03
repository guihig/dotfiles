vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost install.lua source <afile> | PackerCompile
  augroup end
]])

local ensure_packer = function()
	local fn = vim.fn
	local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
	if fn.empty(fn.glob(install_path)) > 0 then
		fn.system({
			"git",
			"clone",
			"--depth",
			"1",
			"https://github.com/wbthomason/packer.nvim",
			install_path,
		})
		vim.cmd([[packadd packer.nvim]])
		return true
	end
	return false
end

local packer_bootstrap = ensure_packer()

return require("packer").startup(function(use)
	use("wbthomason/packer.nvim")

	-- Colorschemes
	use({ "sainnhe/gruvbox-material" })

	-- Utils
	use({
		"beauwilliams/focus.nvim",
		config = function()
			require("focus").setup()
		end,
	})
	use({ "kyazdani42/nvim-web-devicons" })
	use({ "famiu/bufdelete.nvim" })
	use({ "noib3/nvim-cokeline" })
	use({ "hoob3rt/lualine.nvim" })
	use({ "ggandor/lightspeed.nvim", requires = { "tpope/vim-repeat", opt = true } })
	use({ "simeji/winresizer" })
	use({ "kevinhwang91/rnvimr" })
	use({ "psliwka/vim-smoothie" })
	use({
		"nvim-telescope/telescope.nvim",
		requires = { { "nvim-lua/plenary.nvim" } },
	})
	use({
		"iamcco/markdown-preview.nvim",
		run = function()
			vim.fn["mkdp#util#install"]()
		end,
	})
	use({ "tversteeg/registers.nvim" })

	-- Code Utils
	use({ "sjl/tis100.vim" })
	use({ "vimwiki/vimwiki" })
	use({ "jidn/vim-dbml" })
	use({ "ElPiloto/telescope-vimwiki.nvim" })
	use({ "tpope/vim-surround" })
	use({ "JoosepAlviste/nvim-ts-context-commentstring" })
	use({ "numToStr/Comment.nvim" })
	use({ "windwp/nvim-autopairs" })
	-- use { "elixir-editors/vim-elixir" }
	use({ "onsails/lspkind-nvim" })
	use({ "tpope/vim-fugitive" })
	use({ "meain/vim-printer" })
	use({ "lewis6991/gitsigns.nvim" })
	use({ "j-hui/fidget.nvim", branch = "legacy" })
	use({ "vim-test/vim-test" })
	use({ "weilbith/nvim-code-action-menu", cmd = "CodeActionMenu" })
	use({
		"nvim-neotest/neotest",
		requires = {
			"jfpedroza/neotest-elixir",
			"nvim-lua/plenary.nvim",
			"nvim-treesitter/nvim-treesitter",
			"antoinemadec/FixCursorHold.nvim",
		},
	})

	-- Snippets
	use({ "hrsh7th/vim-vsnip" })

	-- Shl
	use({ "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" })

	-- LSP
	use({
		"nvimdev/lspsaga.nvim",
		after = "nvim-lspconfig",
		config = function()
			require("lazy_plugins.init_lspsaga")
		end,
	})
	use({ "williamboman/mason.nvim" })
	use({ "williamboman/mason-lspconfig.nvim" })
	use({
		"hrsh7th/nvim-cmp",
		requires = {
			{ "hrsh7th/cmp-nvim-lsp" },
			{ "hrsh7th/cmp-path" },
			{ "hrsh7th/cmp-nvim-lua" },
			{ "hrsh7th/cmp-vsnip" },
			{ "hrsh7th/cmp-buffer" },
		},
	})
	use({ "neovim/nvim-lspconfig" })

	-- Formatter
	use({ "mhartington/formatter.nvim" })

	-- Test
	use({ "vim-test/vim-test" })

	if packer_bootstrap then
		require("packer").sync()
	end
end)
