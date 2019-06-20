" autoformat configuration
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-04-03

let s:save_cpo = &cpo
set cpo&vim

if !exists('g:loaded_autoformat')
  finish
endif

let g:autoformat_filetypes = ['css', 'json']

let &cpo = s:save_cpo
unlet s:save_cpo
