" Auto format files on write
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-04-03

let s:cpo_save = &cpo
set cpo&vim

if exists('g:loaded_autoformat')
  finish
endif
let g:loaded_autoformat = 1

if !exists('g:autoformat_filetypes')
  let g:autoformat_filetypes = []
endif

augroup plugin.autoformat
  autocmd!
  autocmd BufRead *
        \ if !empty(&l:formatprg) && index(g:autoformat_filetypes, &ft) > -1 |
        \   let b:autoformat = 1 |
        \ endif
  autocmd BufWritePre *
        \ if get(b:, 'autoformat') |
        \   let view = winsaveview() |
        \   execute '%!' . &l:formatprg |
        \   call winrestview(view) |
        \   unlet view |
        \ endif
augroup END

let &cpo = s:cpo_save
unlet s:cpo_save
