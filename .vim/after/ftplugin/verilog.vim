" verilog filetype plugin
" Author: Greg Anders <greg@gpanders.com>

let b:undo_ftplugin = get(b:, 'undo_ftplugin')

setlocal include=^\\s*`include

nnoremap <buffer> <silent> <Bslash>d :<C-U>call ft#verilog#toggle_debug()<CR>

let b:undo_ftplugin .= '|setl inc<|nun <buffer> <Bslash>d'
