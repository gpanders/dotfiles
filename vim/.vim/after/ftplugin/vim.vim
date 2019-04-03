" Vim
setlocal foldmethod=marker

" Z: execute current line or visually selected region in command mode
nnoremap <buffer> Z: ^"zyg_:<C-R>z<CR>
vnoremap <buffer> Z: "zy:<C-U><C-R>=substitute(@z, "\n", "<Bar>", "g")<CR><BS>

" Source current file with c<Enter>
nnoremap <silent> <buffer> c<CR> :source %<Bar>redraw!<CR>

let b:undo_ftplugin .= '|setl fdm<|unm <buffer> Z:|unm <buffer> c<CR>'
