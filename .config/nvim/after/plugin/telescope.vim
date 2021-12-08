if !get(g:, 'loaded_telescope')
    finish
endif

nnoremap <Space>f <Cmd>Telescope find_files hidden=true follow=true<CR>
nnoremap <Space>g <Cmd>Telescope git_files show_untracked=false<CR>
nnoremap <Space>/ <Cmd>Telescope current_buffer_fuzzy_find<CR>
nnoremap <Space>b <Cmd>Telescope buffers<CR>
nnoremap <Space>o <Cmd>Telescope oldfiles<CR>
nnoremap <Space>q <Cmd>Telescope quickfix<CR>
nnoremap z= <Cmd>Telescope spell_suggest<CR>

augroup my_telescope
    autocmd!
    autocmd User LspAttached nnoremap <buffer> <Space>t <Cmd>Telescope lsp_dynamic_workspace_symbols<CR>
augroup END
