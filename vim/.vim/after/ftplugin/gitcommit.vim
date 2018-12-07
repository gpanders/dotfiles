if &filetype !=# 'gitcommit'
  finish
endif

setlocal colorcolumn=73
setlocal textwidth=72

" warning if first line too long
match ErrorMsg /\%1l.\%>51v/

" enable spell check
setlocal spell

let b:undo_ftplugin .= '|setl cc< tw<'
