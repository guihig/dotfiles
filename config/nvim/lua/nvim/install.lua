vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost install.lua source <afile> | PackerCompile
  augroup end
]])

local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({
        "git",
        "clone",
        "--depth",
        "1",
        "https://github.com/wbthomason/packer.nvim",
        install_path
    })
    vim.cmd "packadd packer.nvim"
end

-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]

return require("packer").startup(function(use)
    use "wbthomason/packer.nvim"

    -- Colorschemes
    -- use {'sainnhe/sonokai'}
    use { "sainnhe/gruvbox-material" }

    -- Utils
    use {
        "beauwilliams/focus.nvim",
        config = function() require("focus").setup() end
    }
    use { "kyazdani42/nvim-web-devicons" }
    use { "dstein64/vim-startuptime" }
    use { "famiu/bufdelete.nvim" }
    use { "romgrk/barbar.nvim" }
    use {
        "hoob3rt/lualine.nvim",
        requires = { "kyazdani42/nvim-web-devicons", opt = true }
    }
    use {
        "ggandor/lightspeed.nvim",
        requires = { "tpope/vim-repeat", opt = true }
    }
    use { "mhinz/vim-startify" }
    use { "simeji/winresizer" }
    use { "kevinhwang91/rnvimr" }
    use { "psliwka/vim-smoothie" }
    use {
        "nvim-telescope/telescope.nvim",
        requires = { { "nvim-lua/plenary.nvim" } }
    }
    use({
        "iamcco/markdown-preview.nvim",
        run = function() vim.fn["mkdp#util#install"]() end
    })
    use { "tversteeg/registers.nvim" }

    -- Code Utils
    use { "tpope/vim-surround" }
    use { "JoosepAlviste/nvim-ts-context-commentstring" }
    -- use {'b3nj5m1n/kommentary'}
    use { "numToStr/Comment.nvim" }
    use { "windwp/nvim-autopairs" }
    -- use {'elixir-editors/vim-elixir'}
    use { "onsails/lspkind-nvim" }
    use { "tpope/vim-fugitive" }
    use { "ray-x/lsp_signature.nvim" }
    use { "ThePrimeagen/harpoon", requires = "nvim-lua/plenary.nvim" }
    use { "vimwiki/vimwiki" }
    use { "ElPiloto/telescope-vimwiki.nvim" }
    use { "meain/vim-printer" }

    -- Snippets
    -- use {'rafamadriz/friendly-snippets'}
    -- use {'L3MON4D3/LuaSnip'}
    use { "hrsh7th/vim-vsnip" }

    -- Shl
    use {
        "nvim-treesitter/nvim-treesitter",
        run = ":TSUpdate",
        requires = "p00f/nvim-ts-rainbow"
    }
    use { "nvim-treesitter/playground" }

    -- LSP
    -- use {'neoclide/coc.nvim', branch = "release"}
    use { "petertriho/cmp-git", requires = "nvim-lua/plenary.nvim" }
    use {
        "hrsh7th/nvim-cmp",
        requires = {
            { "hrsh7th/cmp-nvim-lsp" },
            { "hrsh7th/cmp-path" },
            { "hrsh7th/cmp-nvim-lua" },
            { "hrsh7th/cmp-vsnip" },
            { "hrsh7th/cmp-buffer" },
            { "hrsh7th/cmp-emoji" }
        }
    }
    use { "neovim/nvim-lspconfig" }
    use { "glepnir/lspsaga.nvim", branch = "main" }

    -- use {'~/dev/neovim-plugins/lspsaga.nvim', branch = "main"}

    -- Formatter
    use { "mhartington/formatter.nvim" }

    -- Test
    use { "vim-test/vim-test" }
end)
