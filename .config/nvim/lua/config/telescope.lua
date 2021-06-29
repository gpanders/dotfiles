local opts = require('telescope.themes').get_dropdown({
    previewer = false,
    width = 100,
})

require('telescope').setup({
    defaults = {
        mappings = {
            i = {
                ["<C-u>"] = false,
            },
        },
    },
    pickers = {
        find_files = opts,
        buffers = opts,
        oldfiles = opts,
        tags = opts,
        lsp_dynamic_workspace_symbols = opts,
        git_files = opts,
    },
})
