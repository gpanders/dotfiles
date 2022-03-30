if !get(g:, 'loaded_oldfiles')
    finish
endif

let g:oldfiles_blacklist = ['COMMIT_EDITMSG', 'vim/runtime/doc']
