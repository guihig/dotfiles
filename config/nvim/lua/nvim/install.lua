return require('packer').startup(function(use)
    use 'wbthomason/packer.nvim'

    -- Colorschemes
    use {'sainnhe/sonokai'}

    -- Utils
    use {'kyazdani42/nvim-web-devicons'}
    use {'dstein64/vim-startuptime'}
    use {'romgrk/barbar.nvim'}
    use {"glepnir/galaxyline.nvim"}
    use {'mhinz/vim-startify'}
    use {'simeji/winresizer'}
    use {'kevinhwang91/rnvimr'}
    use {'psliwka/vim-smoothie'}
    use {'edkolev/tmuxline.vim'}
    use {'mg979/vim-visual-multi', branch = 'master'}
    use {
        'nvim-telescope/telescope.nvim',
        requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}}
    }

    -- Code Utils
    use {'tpope/vim-dispatch'}
    use {'tpope/vim-surround'}
    use {'preservim/nerdcommenter'}
    use {'Raimondi/delimitMate'}
    use {'mattn/emmet-vim'}

    -- Snippets
    use {'hrsh7th/vim-vsnip'}
    use {'rafamadriz/friendly-snippets'}

    -- Shl
    use {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'}
    use {'elixir-editors/vim-elixir'}

    -- Code Config
    use {'neovim/nvim-lspconfig'}
    -- Using nvim-compe in favor of https://github.com/neovim/neovim/pull/13979
    use {'hrsh7th/nvim-compe'}
    use {
        "mhartington/formatter.nvim",
        opt = true,
        config = function() require("nvim.plugins.formatter").setup() end,
        cmd = {"Format"}
    }
    use {'glepnir/lspsaga.nvim'}
    -- use {'vim-test/vim-test'}
    -- use {
    --     "folke/lsp-trouble.nvim",
    --     requires = "kyazdani42/nvim-web-devicons",
    --     config = function()
    --         require("trouble").setup {
    --             -- your configuration comes here
    --             -- or leave it empty to use the default settings
    --             -- refer to the configuration section below
    --         }
    --     end
    -- }
end)
