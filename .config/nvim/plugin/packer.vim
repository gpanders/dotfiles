function! s:update()
    delcommand PackerUpdate
lua<<
    local ok, packer = pcall(require, "packer")
    if not ok then
        local install_path = vim.fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
        local out = vim.fn.system({ "git", "clone", "https://github.com/wbthomason/packer.nvim", install_path })
        assert(vim.v.shell_error == 0, out)
        packer = require("packer")
    end

    packer.startup {{
        "wbthomason/packer.nvim",
        "dcampos/nvim-snippy",
        "gpanders/editorconfig.nvim",
        "gpanders/fennel-repl.nvim",
        "gpanders/nvim-moonwalk",
        "gpanders/nvim-parinfer",
        "gpanders/vim-medieval",
        "gpanders/vim-oldfiles",
        "tpope/vim-surround",
        "tpope/vim-commentary",
        "tpope/vim-repeat",
        "tpope/vim-rsi",
        { "tpope/vim-scriptease", opt = true },
        "tpope/vim-fugitive",
        "tpope/vim-sleuth",
        "tpope/vim-obsession",
        "justinmk/vim-dirvish",
        "justinmk/vim-sneak",
        "junegunn/vim-easy-align",
        "lewis6991/gitsigns.nvim",
        "mfussenegger/nvim-lint",
        "mfussenegger/nvim-lsp-compl",
        "mfussenegger/nvim-dap",
        "mfussenegger/nvim-dap-python",
        { "neovim/nvim-lspconfig", opt = true },
        { "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" },
        "nvim-treesitter/nvim-treesitter-refactor",
        "nvim-treesitter/nvim-treesitter-textobjects",
        { "nvim-treesitter/playground", opt = true },
        "nvim-lua/plenary.nvim", -- Dependency of gitsigns and telescope
        { "nvim-telescope/telescope.nvim", opt = true },
        "andymass/vim-matchup",

        -- Language specific
        "ziglang/zig.vim",
    }}
    packer.update()
.
endfunction

command! PackerUpdate call s:update()
