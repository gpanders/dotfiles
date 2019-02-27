" Vim
if &filetype !=# 'vim'
  finish
endif

setlocal foldmethod=marker

" Z: execute current line or visually selected region in command mode
nnoremap <buffer> Z: ^"zyg_:<C-R>z<CR>
vnoremap <buffer> Z: "zy:<C-U><C-R>=substitute(@z, "\n", "<Bar>", "g")<CR><BS>

let b:undo_ftplugin .= '|setl fdm<'
