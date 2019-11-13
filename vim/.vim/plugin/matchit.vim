" Get matchit.vim, one way or another
if has('nvim')
    " neovim already loads matchit by default
    finish
endif

if has('packages')
    packadd matchit
else
    silent! runtime macros/matchit.vim
endif
