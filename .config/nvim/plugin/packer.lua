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
        "gpanders/vim-oldfiles",
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
        "romainl/vim-qf",
        "mfussenegger/nvim-lsp-compl",
        { "lewis6991/gitsigns.nvim", requires = { "nvim-lua/plenary.nvim" } },
        { "nvim-telescope/telescope.nvim", opt = true, requires = { "nvim-lua/plenary.nvim" } },

        -- Language plugins
        "ziglang/zig.vim",
    }})
    packer.update()
end, {})
