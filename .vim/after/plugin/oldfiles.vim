if !get(g:, 'loaded_oldfiles')
    finish
endif

let g:oldfiles_blacklist = ['COMMIT_EDITMSG', 'vim/runtime/doc']

augroup plugin_oldfiles
    autocmd!
    autocmd FileType qf if w:quickfix_title =~# 'Oldfiles' | nnoremap <buffer> <CR> <CR>:cclose<CR> | endif
augroup END
