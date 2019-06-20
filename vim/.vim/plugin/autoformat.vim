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
        \ if get(g:, 'autoformat', 1) && get(b:, 'autoformat') |
        \   let view = winsaveview() |
        \   execute '%!' . &l:formatprg |
        \   call winrestview(view) |
        \   unlet view |
        \ endif
augroup END

function! s:toggle(bang)
  let enabled = !get(a:bang ? g: : b:, 'autoformat')
  if a:bang
    let g:autoformat = enabled
    echo 'Automatic formatting ' . (enabled ? 'enabled' : 'disabled') . ' globally'
  else
    let b:autoformat = enabled
    echo 'Automatic formatting ' . (enabled ? 'enabled' : 'disabled') . ' in the current buffer'
  endif
endfunction

command! -nargs=0 -bang Autofmt call <SID>toggle(<bang>0)

let &cpo = s:cpo_save
unlet s:cpo_save
