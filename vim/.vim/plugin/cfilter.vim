" Load cfilter plugin shipped with vim
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-05-30

" Lazy load plugin when quickfix window is first opened
augroup plugin.cfilter
  autocmd!
  autocmd QuickFixCmdPost * silent! packadd cfilter | autocmd! plugin.cfilter
augroup END
