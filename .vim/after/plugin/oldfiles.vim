if !get(g:, 'loaded_oldfiles')
    finish
endif

let g:oldfiles_blacklist = ['\.git/COMMIT_EDITMSG', 'vim/runtime/doc']
