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
        "tpope/vim-surround",
        "tpope/vim-commentary",
        "tpope/vim-repeat",
        "tpope/vim-abolish",
        "tpope/vim-eunuch",
        "tpope/vim-rsi",
        { "tpope/vim-scriptease", opt = true },
        "tpope/vim-fugitive",
        "tpope/vim-sleuth",
        "tpope/vim-obsession",
        "tpope/vim-dispatch",
        "justinmk/vim-dirvish",
        "junegunn/vim-easy-align",
        "mfussenegger/nvim-lsp-compl",
        "lewis6991/gitsigns.nvim",
        "nvim-lua/plenary.nvim", -- Required by telescope
        { "nvim-telescope/telescope.nvim", opt = true, branch = "0.1.x" },
        "nvim-telescope/telescope-fzy-native.nvim",
        "ii14/exrc.vim",

        -- Language plugins
        "ziglang/zig.vim",
    }})
    packer.update()
end, {})
