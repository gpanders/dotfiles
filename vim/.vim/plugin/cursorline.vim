" Highlight the current line in the current window but disable in Insert mode
" Author: Greg Anders

if exists('g:loaded_cursorline')
  finish
endif
let g:loaded_cursorline = 1

let g:cursorline_blacklist = ['tex']

function! s:EnableCursorLine()
  if index(g:cursorline_blacklist, &ft) < 0 && exists('b:cul')
    let &l:cursorline = b:cul
  endif
endfunction

function! s:DisableCursorLine()
  if index(g:cursorline_blacklist, &ft) < 0
    let b:cul = &l:cursorline
    let &l:cursorline = 0
  endif
endfunction

augroup cursorline
  autocmd!
  autocmd InsertEnter,WinLeave * call s:DisableCursorLine()
  autocmd InsertLeave,WinEnter * call s:EnableCursorLine()
augroup END
