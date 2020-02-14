setlocal nomodifiable
setlocal nowrap

" Jump to tag on current line
nnoremap <buffer> <CR> 0:tag <C-R><C-W><CR>

" Open tag on current line in preview window
nnoremap <buffer> p 0<C-W>}

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '') . '|setl ma< wrap<|nun <buffer> <CR>|nun <buffer> p'
