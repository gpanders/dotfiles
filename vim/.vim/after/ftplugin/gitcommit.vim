" git commit filetype plugin
" Author: Greg Anders <greg@gpanders.com>

" enable spell check
setlocal spell
setlocal textwidth=72
setlocal comments+=fb:*,fb:-,fb:+
setlocal formatoptions+=cn
setlocal formatlistpat=^\\s*\\d\\+\\.\\s\\+\\\|^[-*+]\\s\\+\\\|^\\[^\\ze[^\\]]\\+\\]:

" warning if first line too long
match ErrorMsg /\%1l.\%>51v/

augroup ftplugin.gitcommit
  autocmd!
  " Always place cursor at first column of first line
  autocmd BufWinEnter <buffer> call cursor(1, 1)
augroup END

let b:undo_ftplugin .= '|setl tw< spell< com< fo< flp<'
let b:undo_ftplugin .= '|exe "au! ftplugin.gitcommit * <buffer>"'
