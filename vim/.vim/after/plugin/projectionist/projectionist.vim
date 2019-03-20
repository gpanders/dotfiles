" vim-projectionist configuration
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-01-04

if !get(g:, 'loaded_projectionist', 0)
  finish
endif

if !exists('g:projectionist_transformations')
  let g:projectionist_transformations = {}
endif

function! g:projectionist_transformations.date(input, o) abort
  return strftime('%Y-%m-%d')
endfunction
