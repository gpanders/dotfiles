(local install-path
       (.. (vim.fn.stdpath :data) "/site/pack/packer/start/packer.nvim"))

(when (> (vim.fn.empty (vim.fn.glob install-path)) 0)
  (vim.fn.system ["git" "clone" "https://github.com/wbthomason/packer.nvim" install-path]))

(macro use [name ?opts]
  (if ?opts
      (do
        (table.insert ?opts name)
        `,?opts)
      `,name))

(local packer (require :packer))

(packer.startup [[
  (use "wbthomason/packer.nvim")
  (use "gpanders/vim-medieval")
  (use "gpanders/vim-oldfiles")
  (use "gpanders/editorconfig.nvim")
  (use "gpanders/snippets.nvim")
  (use "tpope/vim-surround")
  (use "tpope/vim-commentary")
  (use "tpope/vim-repeat")
  (use "tpope/vim-rsi")
  (use "tpope/vim-scriptease" {:opt true})
  (use "tpope/vim-fugitive")
  (use "tpope/vim-sleuth")
  (use "tpope/vim-obsession")
  (use "justinmk/vim-dirvish")
  (use "junegunn/vim-easy-align")
  (use "lewis6991/gitsigns.nvim")
  (use "mfussenegger/nvim-lint")
  (use "ludovicchabant/vim-gutentags")
  (use "gpanders/vim-scdoc")
  (use "neovim/nvim-lspconfig" {:opt true})
  (use "hrsh7th/nvim-compe" {:opt true})
  (use "nvim-treesitter/nvim-treesitter" {:run ":TSUpdate"})
  (use "nvim-treesitter/nvim-treesitter-refactor")
  (use "nvim-treesitter/playground" {:opt true})
  (use "nvim-lua/popup.nvim")
  (use "nvim-lua/plenary.nvim")
  (use "nvim-telescope/telescope.nvim" {:opt true})
]])

(packer.update)
