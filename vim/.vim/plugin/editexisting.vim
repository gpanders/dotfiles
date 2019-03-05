" Load editexisting.vim
if has('packages') && !has('nvim')
  packadd! editexisting
else
  silent! runtime macros/editexisting.vim
endif
