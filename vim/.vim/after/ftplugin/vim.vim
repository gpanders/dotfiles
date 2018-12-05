" Vim
if &filetype !=# 'vim'
  finish
endif

setlocal foldmethod=marker

let b:undo_ftplugin .= '|setlocal fdm<'
