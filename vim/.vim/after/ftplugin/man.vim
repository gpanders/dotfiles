" man filetype plugin
" Author: Greg Anders <greg@gpanders.com>

nnoremap <buffer> <silent> <C-J> /\C\%>1l\f\+([1-9][a-z]\=)\ze\_.\+\%$<CR>:<C-U>nohlsearch<CR>
nnoremap <buffer> <silent> <C-K> ?\C\%>1l\f\+([1-9][a-z]\=)\ze\_.\+\%$<CR>:<C-U>nohlsearch<CR>

let b:undo_ftplugin .= '|nun <buffer> <C-J>|nun <buffer> <C-K>'
