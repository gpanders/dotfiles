if !get(g:, 'loaded_telescope')
    finish
endif

nnoremap <Space>ff <Cmd>Telescope find_files theme=dropdown previewer=false hidden=true follow=true<CR>
nnoremap <Space>fb <Cmd>Telescope buffers theme=dropdown previewer=false<CR>
nnoremap <Space>fo <Cmd>Telescope oldfiles theme=dropdown previewer=false<CR>
nnoremap <Space>fl <Cmd>Telescope loclist<CR>
nnoremap <Space>fq <Cmd>Telescope quickfix<CR>
nnoremap z= <Cmd>Telescope spell_suggest theme=cursor<CR>

augroup telescope#
    autocmd!
    autocmd User LspAttached nnoremap <buffer> <Space>t <Cmd>Telescope lsp_dynamic_workspace_symbols theme=dropdown previewer=false<CR>
augroup END
