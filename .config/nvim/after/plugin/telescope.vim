if !get(g:, 'loaded_telescope')
    finish
endif

nnoremap <Space>f <Cmd>Telescope find_files theme=get_dropdown previewer=false width=100 hidden=true follow=true<CR>
nnoremap <Space>g <Cmd>Telescope live_grep<CR>
nnoremap <Space>b <Cmd>Telescope buffers theme=get_dropdown previewer=false width=100<CR>
nnoremap <Space>o <Cmd>Telescope oldfiles theme=get_dropdown previewer=false width=100<CR>
nnoremap <Space>t <Cmd>Telescope tags theme=get_dropdown previewer=false width=100<CR>
nnoremap <Space>q <Cmd>Telescope quickfix<CR>
nnoremap z= <Cmd>Telescope spell_suggest<CR>

augroup my_telescope
    autocmd!
    autocmd User LspAttached nnoremap <buffer> <Space>t <Cmd>Telescope lsp_dynamic_workspace_symbols theme=get_dropdown previewer=false width=100<CR>
    autocmd User Fugitive nnoremap <buffer> <Space>f <Cmd>Telescope git_files show_untracked=false theme=get_dropdown previewer=false width=100<CR>
augroup END
