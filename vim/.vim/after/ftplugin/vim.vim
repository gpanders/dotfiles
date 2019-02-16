" Vim
if &filetype !=# 'vim'
  finish
endif

setlocal foldmethod=marker

" Z: execute line in command mode
nnoremap <buffer> Z: ^"zyg_:<C-R>z<CR>
vnoremap <buffer> Z: "zy:<C-R>=substitute(@z, "\n", "<Bar>", "g")<CR><BS><CR>

let b:undo_ftplugin .= '|setlocal fdm<'
