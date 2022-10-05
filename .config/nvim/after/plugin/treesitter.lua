if not vim.g.loaded_nvim_treesitter then
    return
end

require("nvim-treesitter.configs").setup({
    highlight = {
        enable = true,
        disable = {
            "help", -- Until https://github.com/neovim/tree-sitter-vimdoc/issues/23 is resolved
        },
    },
})
