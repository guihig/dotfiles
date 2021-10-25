vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost install.lua source <afile> | PackerCompile
  augroup end
]])

local fn = vim.fn
local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({
        'git', 'clone', '--depth', '1',
        'https://github.com/wbthomason/packer.nvim', install_path
    })
    vim.cmd 'packadd packer.nvim'
end

-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
    use 'wbthomason/packer.nvim'

    -- Colorschemes
    -- use {'sainnhe/sonokai'}
    use {'sainnhe/gruvbox-material'}

    -- Utils
    use {'kyazdani42/nvim-web-devicons'}
    use {'dstein64/vim-startuptime'}
    use {'romgrk/barbar.nvim'}
    -- use {"glepnir/galaxyline.nvim"}
    use {
        'hoob3rt/lualine.nvim',
        requires = {'kyazdani42/nvim-web-devicons', opt = true}
    }
    use {"easymotion/vim-easymotion"}
    use {'mhinz/vim-startify'}
    use {'simeji/winresizer'}
    use {'kevinhwang91/rnvimr'}
    use {'psliwka/vim-smoothie'}
    use {
        'nvim-telescope/telescope.nvim',
        requires = {{'nvim-lua/plenary.nvim'}}
    }

    -- Code Utils
    use {'tpope/vim-surround'}
    use {'JoosepAlviste/nvim-ts-context-commentstring'}
    use {'b3nj5m1n/kommentary'}
    use {'windwp/nvim-autopairs'}
    use {'elixir-editors/vim-elixir'}

    -- Snippets
    -- use {'rafamadriz/friendly-snippets'}
    use {'L3MON4D3/LuaSnip'}

    -- Shl
    use {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'}

    -- LSP
    -- use {'neoclide/coc.nvim', branch = "release"}
    use {
        'hrsh7th/nvim-cmp',
        requires = {
            {'hrsh7th/cmp-nvim-lsp'}, {'hrsh7th/cmp-path'},
            {'hrsh7th/cmp-nvim-lua'}, {'saadparwaiz1/cmp_luasnip'}
            -- {'hrsh7th/cmp-buffer'}
        }
    }
    use {'neovim/nvim-lspconfig'}
    -- use {'tami5/lspsaga.nvim'}
    use {'~/dev/neovim-plugins/lspsaga.nvim', branch = "main"}

    -- Formatter
    use {'mhartington/formatter.nvim'}

    -- Test
    use {'vim-test/vim-test'}
end)
