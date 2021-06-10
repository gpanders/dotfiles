if !get(g:, 'loaded_telescope')
    finish
endif

lua <<EOF
local opts = require('telescope.themes').get_dropdown {
    width = 100,
    previewer = false,
}

require('telescope').setup {
    defaults = {
        layout_strategy = 'flex',
    },
    pickers = {
        find_files = vim.tbl_extend('force', opts, {
            find_command = vim.split(vim.env.FZF_DEFAULT_COMMAND, ' '),
        }),
        buffers = opts,
        oldfiles = opts,
    },
}
EOF

nnoremap <Space>f <Cmd>Telescope find_files<CR>
nnoremap <Space>g <Cmd>Telescope live_grep<CR>
nnoremap <Space>b <Cmd>Telescope buffers<CR>
nnoremap <Space>o <Cmd>Telescope oldfiles<CR>
nnoremap <Space>t <Cmd>Telescope tags<CR>
nnoremap <Space>q <Cmd>Telescope quickfix<CR>
nnoremap z= <Cmd>Telescope spell_suggest<CR>

augroup my_telescope
    autocmd!
    autocmd User LspAttached nnoremap <buffer> <Space>t <Cmd>Telescope lsp_dynamic_workspace_symbols<CR>
augroup END
