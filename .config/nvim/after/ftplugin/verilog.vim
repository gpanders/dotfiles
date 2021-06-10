setlocal include=^\\s*`include

nnoremap <buffer> <Bslash>d <Cmd>call ft#verilog#toggle_debug()<CR>

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '') . '|setl inc<|nun <buffer> <Bslash>d'
