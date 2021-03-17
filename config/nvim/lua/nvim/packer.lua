local packer_exists = pcall(vim.cmd, [[packadd packer.nvim]])

return require('packer').startup(function(use)
    -- Colorschemes
    use {'tjdevries/colorbuddy.vim'}
    use {'tjdevries/gruvbuddy.nvim'}

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
    use {'airblade/vim-gitgutter'}

    -- Code Utils
    use {'tpope/vim-dispatch'}
    use {'tpope/vim-surround'}
    use {'preservim/nerdcommenter'}
    use {'jiangmiao/auto-pairs'}

    -- Snippets
    use {'honza/vim-snippets'}
    use {'SirVer/ultisnips'}

    -- Shl
    use {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'}
    use {'elixir-editors/vim-elixir'}
    use {'posva/vim-vue'}
    use {'dag/vim-fish'}
    use {'HerringtonDarkholme/yats.vim'}

    -- Code Config
    -- use {'neoclide/coc.nvim', branch = 'release'}
    use {'neovim/nvim-lspconfig'}
    use {'nvim-lua/completion-nvim'}
    use {
        "mhartington/formatter.nvim",
        opt = true,
        config = function() require("nvim.plugins.formatter").setup() end,
        cmd = {"Format"}
    }
    use {'glepnir/lspsaga.nvim'}
    use {'vim-test/vim-test'}
end)
