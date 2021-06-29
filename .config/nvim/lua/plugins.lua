local install_path = vim.fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
    vim.fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path})
end

return require('packer').startup(function(use)
    use 'wbthomason/packer.nvim'

    use 'gpanders/vim-medieval'

    use 'gpanders/vim-oldfiles'

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

    use 'dense-analysis/ale'

    use 'ludovicchabant/vim-gutentags'

    use 'romainl/vim-qlist'

    use 'gpanders/vim-markdown'

    use 'gpanders/vim-scdoc'

    use { 'tweekmonster/startuptime.vim', opt = true }

    use { 'neovim/nvim-lspconfig', opt = true }

    use 'hrsh7th/nvim-compe'

    -- use 'nvim-lua/completion-nvim'

    use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

    use 'nvim-treesitter/nvim-treesitter-refactor'

    use { 'nvim-treesitter/playground', opt = true }

    use 'nvim-lua/popup.nvim'

    use 'nvim-lua/plenary.nvim'

    use { 'nvim-telescope/telescope.nvim', opt = true }

end)
