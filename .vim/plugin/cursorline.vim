" Highlight the current line in the current window but disable in Insert mode
if exists('g:loaded_cursorline') || !has('syntax')
  finish
endif
let g:loaded_cursorline = 1

let g:cursorline_blacklist = ['tex']

augroup cursorline
  autocmd!
  autocmd InsertEnter,WinLeave * call cursorline#toggle(0)
  autocmd InsertLeave,WinEnter * call cursorline#toggle(1)
augroup END
