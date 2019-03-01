if &filetype !=# 'gitcommit'
  finish
endif

" enable spell check
setlocal spell
setlocal textwidth=72

" warning if first line too long
match ErrorMsg /\%1l.\%>51v/

augroup gitcommit_ftplugin
  autocmd! * <buffer>
  autocmd BufWinEnter <buffer> normal gg0
augroup END

let b:undo_ftplugin .= '|setl tw< spell<'
let b:undo_ftplugin .= '|exe "au! gitcommit_ftplugin * <buffer>"'

