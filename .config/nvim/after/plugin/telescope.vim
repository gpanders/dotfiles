if !get(g:, 'loaded_telescope')
    finish
endif

nnoremap <Space>f <Cmd>Telescope find_files hidden=true follow=true<CR>
nnoremap <Space>g <Cmd>Telescope live_grep<CR>
nnoremap <Space>b <Cmd>Telescope buffers<CR>
nnoremap <Space>o <Cmd>Telescope oldfiles<CR>
nnoremap <Space>q <Cmd>Telescope quickfix<CR>
nnoremap z= <Cmd>Telescope spell_suggest<CR>

augroup my_telescope
    autocmd!
    autocmd User LspAttached nnoremap <buffer> <Space>t <Cmd>Telescope lsp_dynamic_workspace_symbols<CR>

    if exists('*FugitiveGitDir')
        autocmd BufNewFile,BufRead * if !empty(FugitiveGitDir()) | nnoremap <buffer> <Space>f <Cmd>Telescope git_files show_untracked=false<CR> | endif
    endif
augroup END
