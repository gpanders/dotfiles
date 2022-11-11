nvim.create_user_command("PackerUpdate", function()
    local ok, packer = pcall(require, "packer")
    if not ok then
        local install_path = string.format("%s/site/pack/packer/start/packer.nvim", vim.fn.stdpath("data"))
        local out = vim.fn.system("git clone https://github.com/wbthomason/packer.nvim " .. install_path)
        assert(vim.v.shell_error == 0, out)
        vim.o.runtimepath = vim.o.runtimepath
        packer = require("packer")
    end

    packer.startup({{
        "wbthomason/packer.nvim",
        "gpanders/nvim-parinfer",
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
        "justinmk/vim-dirvish",
        "junegunn/vim-easy-align",
        "mfussenegger/nvim-lsp-compl",
        "lewis6991/gitsigns.nvim",
        "nvim-lua/plenary.nvim", -- Required by telescope
        { "nvim-telescope/telescope.nvim", opt = true, branch = "0.1.x" },
        "nvim-telescope/telescope-fzy-native.nvim",
        { "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" },
        { "nvim-treesitter/playground", opt = true },
        "ii14/exrc.vim",
        { "dcampos/nvim-snippy", opt = true },

        -- Language plugins
        "ziglang/zig.vim",
    }})
    packer.update()
end, {})
