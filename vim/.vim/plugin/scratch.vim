" Provide a :Scratch command to view output of any ex command in a scratch
" buffer
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-03-03

if exists('g:loaded_scratch')
  finish
endif
let g:loaded_scratch = 1

command! -nargs=+ Scratch call scratch#open(<f-args>)
