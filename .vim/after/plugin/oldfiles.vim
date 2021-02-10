if !get(g:, 'loaded_oldfiles')
    finish
endif

let g:oldfiles_blacklist = ['COMMIT_EDITMSG', 'vim/runtime/doc']

augroup plugin_oldfiles
    autocmd!
    autocmd FileType qf autocmd WinLeave <buffer> ++nested if getqflist({'title': 0}).title =~# 'Oldfiles' | cclose | endif
augroup END
