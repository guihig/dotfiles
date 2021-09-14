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
    use {'sainnhe/sonokai'}

    -- Utils
    use {'kyazdani42/nvim-web-devicons'}
    use {'dstein64/vim-startuptime'}
    use {'romgrk/barbar.nvim'}
    use {"glepnir/galaxyline.nvim"}
    use {"easymotion/vim-easymotion"}
    use {'mhinz/vim-startify'}
    use {'simeji/winresizer'}
    use {'kevinhwang91/rnvimr'}
    use {'psliwka/vim-smoothie'}
    use {'edkolev/tmuxline.vim'}
    use {
        'nvim-telescope/telescope.nvim',
        requires = {{'nvim-lua/plenary.nvim'}}
    }

    -- Code Utils
    use {'tpope/vim-dispatch'}
    use {'tpope/vim-surround'}
    use {'JoosepAlviste/nvim-ts-context-commentstring'}
    use {'b3nj5m1n/kommentary'}
    use {'Raimondi/delimitMate'}

    -- Snippets
    -- use {'hrsh7th/vim-vsnip'}
    -- use {'rafamadriz/friendly-snippets'}

    -- Shl
    use {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'}

    -- Code Config
    use {'neovim/nvim-lspconfig'}
    -- Using nvim-compe in favor of https://github.com/neovim/neovim/pull/13979
    use {'hrsh7th/nvim-compe'}
    use {'mhartington/formatter.nvim'}
    use {'glepnir/lspsaga.nvim'}
    use {'vim-test/vim-test'}
end)
