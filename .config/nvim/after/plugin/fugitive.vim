if !get(g:, 'loaded_fugitive')
  finish
endif

augroup plugin_fugitive
    autocmd!
    autocmd BufRead fugitive://* setlocal bufhidden=delete
    autocmd FileType fugitive,fugitiveblame nmap <silent> <buffer> q gq
    autocmd BufRead * if getcwd() ==# $HOME && (empty('b:git_dir') || b:git_dir ==# '') | let b:git_dir = $HOME . '/.dotfiles' | endif
augroup END

nnoremap g<Space> :Git<Space>
nnoremap g<CR> <Cmd>Git<CR>
