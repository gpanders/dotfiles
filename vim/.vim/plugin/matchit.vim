" Get matchit.vim, one way or another
if has('packages') && !has('nvim')
  packadd! matchit
else
  silent! runtime macros/matchit.vim
endif
