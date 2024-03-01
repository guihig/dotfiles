require("lazy").setup({
	-- Colorschemes
	{ "sainnhe/gruvbox-material" },

	-- UI
	{ "nvim-tree/nvim-web-devicons" },
	{ "noib3/nvim-cokeline" },
	{ "hoob3rt/lualine.nvim" },

	-- FS
	{ "kevinhwang91/rnvimr" },
	{
		"nvim-telescope/telescope.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
	},
	{ "ElPiloto/telescope-vimwiki.nvim" },
	{ "nvim-telescope/telescope-fzy-native.nvim" },
	{ "vimwiki/vimwiki" },

	-- Utils
	{
		"folke/which-key.nvim",
		event = "VeryLazy",
		init = function()
			vim.o.timeout = true
			vim.o.timeoutlen = 300
		end,
	},
	{
		"beauwilliams/focus.nvim",
		config = function()
			require("focus").setup()
		end,
	},
	{ "famiu/bufdelete.nvim" },
	{ "ggandor/lightspeed.nvim", dependencies = { "tpope/vim-repeat", opt = true } },
	{ "simeji/winresizer" },
	{ "psliwka/vim-smoothie" },
	{ "tversteeg/registers.nvim" },
	{
		"iamcco/markdown-preview.nvim",
		build = function()
			vim.fn["mkdp#util#install"]()
		end,
	},

	-- Code
	{
		"norcalli/nvim-colorizer.lua",
		config = function()
			require("colorizer").setup()
		end,
	},
	{ "jidn/vim-dbml" },
	{ "tpope/vim-surround" },
	{ "JoosepAlviste/nvim-ts-context-commentstring" },
	{ "numToStr/Comment.nvim" },
	{ "windwp/nvim-autopairs" },
	{ "onsails/lspkind-nvim" },
	{ "tpope/vim-fugitive" },
	{ "akinsho/git-conflict.nvim" },
	{ "meain/vim-printer" },
	{ "lewis6991/gitsigns.nvim" },
	{ "j-hui/fidget.nvim", branch = "legacy" },
	{ "vim-test/vim-test" },
	{
		"nvim-neotest/neotest",
		dependencies = {
			"jfpedroza/neotest-elixir",
			"nvim-lua/plenary.nvim",
			"nvim-treesitter/nvim-treesitter",
			"antoinemadec/FixCursorHold.nvim",
		},
	},
	{
		"rest-nvim/rest.nvim",
		dependencies = { { "nvim-lua/plenary.nvim" } },
	},

	-- Snippets
	{ "L3MON4D3/LuaSnip", version = "v2.*", dependencies = { "rafamadriz/friendly-snippets" } },

	-- SHL
	{ "nvim-treesitter/nvim-treesitter", build = ":TSUpdate" },

	-- LSP
	{ "folke/neodev.nvim", opts = {} },
	{ "williamboman/mason.nvim" },
	{ "williamboman/mason-lspconfig.nvim" },
	{ "VonHeikemen/lsp-zero.nvim", branch = "v3.x" },
	{ "nvimdev/lspsaga.nvim" },
	{
		"hrsh7th/nvim-cmp",
		dependencies = {
			{ "hrsh7th/cmp-nvim-lsp" },
			{ "FelipeLema/cmp-async-path" },
			{ "hrsh7th/cmp-nvim-lua" },
			{ "saadparwaiz1/cmp_luasnip" },
			{ "hrsh7th/cmp-buffer" },
		},
	},
	{ "neovim/nvim-lspconfig" },

	-- Formatter
	{ "mhartington/formatter.nvim" },

	-- Test
	{ "vim-test/vim-test" },
})
