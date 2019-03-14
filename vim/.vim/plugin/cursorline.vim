" Highlight the current line in the current window but disable in Insert mode
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-02-25

if exists('g:loaded_cursorline')
  finish
endif
let g:loaded_cursorline = 1

let g:cursorline_blacklist = ['tex']

augroup plugin.cursorline
  autocmd!
  autocmd InsertEnter,WinLeave * call cursorline#toggle(v:false)
  autocmd InsertLeave,WinEnter * call cursorline#toggle(v:true)
augroup END
