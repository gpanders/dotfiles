local fn = vim.fn
local install_path = fn.stdpath('data') .. '/site/pack/packer/opt/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path})
end

vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
    use { 'wbthomason/packer.nvim', opt = true }

    use { 'gpanders/base16-vim', opt = true }

    use 'gpanders/vim-medieval'

    use 'tpope/vim-surround'

    use 'tpope/vim-repeat'

    use 'tpope/vim-commentary'

    use 'tpope/vim-rsi'

    use { 'tpope/vim-scriptease', opt = true }

    use 'tpope/vim-fugitive'

    use 'tpope/vim-sleuth'

    use 'tpope/vim-obsession'

    use 'justinmk/vim-dirvish'

    use 'junegunn/vim-easy-align'

    use 'mhinz/vim-signify'

    use 'neomake/neomake'

    use 'ludovicchabant/vim-gutentags'

    use 'romainl/vim-qlist'

    use 'gpanders/vim-markdown'

    use 'gpanders/vim-scdoc'

    use 'neovim/nvim-lspconfig'

    use 'hrsh7th/nvim-compe'

    use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

    use 'nvim-treesitter/nvim-treesitter-refactor'

    use { 'nvim-treesitter/playground', opt = true }

    use {
        'nvim-telescope/telescope.nvim',
        requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}}
    }

end)
