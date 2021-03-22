vim.cmd([[packadd packer.nvim]])

return require('packer').startup(function(use)
    -- Colorschemes
    use {'tjdevries/colorbuddy.vim'}
    use {'tjdevries/gruvbuddy.nvim'}
    use {'sainnhe/sonokai'}

    -- Utils
    use {'kyazdani42/nvim-web-devicons'}
    use {'akinsho/nvim-bufferline.lua'}
    use {"glepnir/galaxyline.nvim"}
    use {'mhinz/vim-startify'}
    use {'simeji/winresizer'}
    use {'easymotion/vim-easymotion'}
    use {'kevinhwang91/rnvimr'}
    use {'psliwka/vim-smoothie'}
    use {'edkolev/tmuxline.vim'}
    use {'mg979/vim-visual-multi', branch = 'master'}
    use {
        'nvim-telescope/telescope.nvim',
        requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}}
    }

    -- Git
    use {'tpope/vim-fugitive'}

    -- Code Utils
    use {'tpope/vim-dispatch'}
    use {'tpope/vim-surround'}
    use {'preservim/nerdcommenter'}
    use {'windwp/nvim-autopairs'}

    -- Snippets
    -- use {'honza/vim-snippets'}
    -- use {'SirVer/ultisnips'}
    use {'hrsh7th/vim-vsnip'}
    use {'hrsh7th/vim-vsnip-integ'}

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
    use {'vim-test/vim-test'}
end)
