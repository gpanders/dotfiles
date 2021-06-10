if exists('g:loaded_cursorline') || !has('syntax')
  finish
endif
let g:loaded_cursorline = 1

augroup cursorline
  autocmd!
  autocmd InsertEnter,WinLeave * let b:cul = &l:cursorline | let &l:cursorline = 0
  autocmd InsertLeave,WinEnter * if exists('b:cul') | let &l:cursorline = b:cul | endif
augroup END
