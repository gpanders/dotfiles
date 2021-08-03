function! s:update()
    delcommand PackerUpdate
lua <<
local install_path = vim.fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
    vim.fn.system({"git", "clone", "https://github.com/wbthomason/packer.nvim", install_path})
end

require("packer").startup(function(use)
    use "wbthomason/packer.nvim"

    use "gpanders/vim-medieval"

    use "gpanders/vim-oldfiles"

    use "gpanders/editorconfig.nvim"

    use "gpanders/snippets.nvim"

    use "gpanders/vim-surround"

    use "tpope/vim-commentary"

    use "tpope/vim-repeat"

    use "tpope/vim-rsi"

    use { "tpope/vim-scriptease", opt = true }

    use "tpope/vim-fugitive"

    use "tpope/vim-sleuth"

    use "tpope/vim-obsession"

    use "justinmk/vim-dirvish"

    use "junegunn/vim-easy-align"

    use "lewis6991/gitsigns.nvim"

    use "mfussenegger/nvim-lint"

    use "ludovicchabant/vim-gutentags"

    use "gpanders/vim-markdown"

    use "gpanders/vim-scdoc"

    use { "neovim/nvim-lspconfig", opt = true }

    use { "hrsh7th/nvim-compe", opt = true }

    use { "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" }

    use "nvim-treesitter/nvim-treesitter-refactor"

    use { "nvim-treesitter/playground", opt = true }

    use "nvim-lua/popup.nvim"

    use "nvim-lua/plenary.nvim"

    use { "nvim-telescope/telescope.nvim", opt = true }

    use "rktjmp/hotpot.nvim"
end)

require("packer").update()
.
endfunction

command PackerUpdate call s:update()
