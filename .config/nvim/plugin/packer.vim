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
        "gpanders/snippets.nvim",
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
        -- lspconfig loads vim.lsp on startup which is big and slow and not
        -- needed unless in a filetype with an LSP server, so defer loading
        -- until one of those filetypes is opened (see plugin/lsp.fnl)
        { "neovim/nvim-lspconfig", opt = true },
        { "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" },
        "nvim-treesitter/nvim-treesitter-refactor",
        "nvim-treesitter/nvim-treesitter-textobjects",
        { "nvim-treesitter/playground", opt = true },
        "nvim-lua/plenary.nvim", -- Dependency of gitsigns and telescope
        -- Configuring telescope means loading the entire plugin, so defer
        -- loading until the :Telescope command is actually used (see
        -- plugin/telescope.fnl)
        { "nvim-telescope/telescope.nvim", opt = true },
        "andymass/vim-matchup",
        "nathom/filetype.nvim",
    }}
    packer.update()
.
endfunction

command! PackerUpdate call s:update()
