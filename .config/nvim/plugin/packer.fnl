(macro use [...]
  (let [spec []
        nargs (+ (select :# ...) 1)]
    (var i 1)
    (while (< i nargs)
      (let [(plugin next) (select i ...)]
        (match (type next)
          :table (do
                   (table.insert spec (collect [k v (pairs next) :into {1 plugin}]
                                        (values k v)))
                   (set i (+ i 1)))
          _ (table.insert spec plugin)))
      (set i (+ i 1)))
    `(do
       (packer.startup [,spec])
       (packer.update))))

(command :PackerUpdate {:force true}
  (fn []
    (vim.api.nvim_del_user_command :PackerUpdate)
    (let [packer (match (pcall require :packer)
                   (true packer) packer
                   false (let [install-path (.. (vim.fn.stdpath :data) "/site/pack/packer/start/packer.nvim")
                               out (vim.fn.system [:git :clone "https://github.com/wbthomason/packer.nvim" install-path])]
                           (assert (= 0 vim.v.shell_error) out)
                           (tset package.loaded :packer nil)
                           (require :packer)))]
      (use
        "wbthomason/packer.nvim"
        "dcampos/nvim-snippy"
        "gpanders/nvim-parinfer"
        "tpope/vim-surround"
        "tpope/vim-commentary"
        "tpope/vim-rsi"
        "tpope/vim-scriptease" {:opt true}
        "tpope/vim-fugitive"
        "tpope/vim-sleuth"
        "tpope/vim-obsession"
        "justinmk/vim-dirvish"
        "junegunn/vim-easy-align"
        "lewis6991/gitsigns.nvim"
        "mfussenegger/nvim-lint"
        "mfussenegger/nvim-lsp-compl"
        "mfussenegger/nvim-dap"
        "mfussenegger/nvim-fzy"
        "nvim-lua/plenary.nvim" ; Dependency of gitsigns and telescope
        "nvim-telescope/telescope.nvim" {:opt true}
        "andymass/vim-matchup"

        ; Language specific
        "ziglang/zig.vim"))))
