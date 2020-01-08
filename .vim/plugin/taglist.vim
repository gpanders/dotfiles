" Show list of tags in the current file
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-04-01

if exists('g:loaded_taglist')
  finish
endif
let g:loaded_taglist = 1

command! -nargs=? Taglist call taglist#open(<f-args>)
