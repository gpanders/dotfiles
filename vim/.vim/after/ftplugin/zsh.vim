" zsh filetype plugin
" Author: Greg Anders <greg@gpanders.com>

if &filetype !=# 'zsh'
  finish
endif

setlocal shiftwidth=2
setlocal softtabstop=2

" Z! execute line as shell command
" Inspired by the Shdo buffers from vim-dirvish
nnoremap <buffer> Z! ^"zyg_:!<C-R>z<CR>

let b:undo_ftplugin = '|setl sw< sts<|nun <buffer> Z!'
