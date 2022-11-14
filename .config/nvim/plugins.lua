local ok, paq = pcall(require, "paq")
if not ok then
    local out = vim.fn.system({
        "git",
        "clone",
        "--depth=1",
        "https://github.com/savq/paq-nvim",
        vim.fn.stdpath("data") .. "/site/pack/paqs/start/paq-nvim",
    })
    assert(vim.v.shell_error == 0, out)
    vim.o.runtimepath = vim.o.runtimepath
    paq = require("paq")
end

paq {
    "https://github.com/savq/paq-nvim",
    "https://github.com/gpanders/nvim-parinfer",
    "https://tpope.io/vim/surround",
    "https://tpope.io/vim/commentary",
    "https://tpope.io/vim/repeat",
    "https://tpope.io/vim/abolish",
    "https://tpope.io/vim/eunuch",
    "https://tpope.io/vim/rsi",
    { "https://tpope.io/vim/scriptease", opt = true },
    "https://tpope.io/vim/fugitive",
    "https://tpope.io/vim/sleuth",
    "https://tpope.io/vim/obsession",
    "https://tpope.io/vim/dispatch",
    "https://github.com/justinmk/vim-dirvish",
    "https://github.com/junegunn/vim-easy-align",
    "https://github.com/mfussenegger/nvim-lsp-compl",
    "https://github.com/lewis6991/gitsigns.nvim",
    "https://github.com/nvim-lua/plenary.nvim", -- Required by telescope
    { "https://github.com/nvim-telescope/telescope.nvim", opt = true, branch = "0.1.x" },
    "https://github.com/nvim-telescope/telescope-fzy-native.nvim",
    { "https://github.com/nvim-treesitter/nvim-treesitter", opt = true, run = function()
        vim.cmd "packadd nvim-treesitter|TSUpdate"
    end }
    { "https://github.com/nvim-treesitter/playground", opt = true },
    "https://github.com/ii14/exrc.vim",
    { "https://github.com/dcampos/nvim-snippy", opt = true },

    -- Language plugins
    "https://github.com/ziglang/zig.vim",
}

paq:sync()
