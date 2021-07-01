let g:loaded_telescope = 1

function! s:load(...)
    delcommand Telescope
    unlet g:loaded_telescope
    packadd telescope.nvim
    if !exists('g:loaded_telescope')
        echohl ErrorMsg
        echo 'telescope.nvim is not installed'
        echohl None
        return
    endif

lua <<
local opts = require("telescope.themes").get_dropdown({
    previewer = false,
    layout_config = {
        width = 100,
    },
})

require("telescope").setup({
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
.

    exec 'Telescope' join(a:000)
endfunction

command -nargs=* Telescope call s:load(<f-args>)
