" man filetype plugin
" Author: Greg Anders <greg@gpanders.com>

if &filetype !=# 'man'
  finish
endif

nnoremap <buffer> <silent> <C-J> /\<\w\+(\d\+)<CR>:<C-U>nohlsearch<CR>
nnoremap <buffer> <silent> <C-K> ?\<\w\+(\d\+)<CR>:<C-U>nohlsearch<CR>

let b:undo_ftplugin .= '|nun <buffer> <C-J>|nun <buffer> <C-K>'
