let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

" enable spell check
setlocal spell
setlocal textwidth=72
setlocal formatoptions+=cn
setlocal formatlistpat=^\\s*\\d\\+\\.\\s\\+\\\|^\\s*[-*+]\\s\\+\\\|^\\[^\\ze[^\\]]\\+\\]:

let b:undo_ftplugin .= '|setl tw< spell< fo< flp<'

" warning if first line too long
match ErrorMsg /\%1l.\%>51v/

augroup gitcommit
  autocmd!
  " Always place cursor at first column of first line
  autocmd BufWinEnter <buffer> call cursor(1, 1)
augroup END
let b:undo_ftplugin .= '|au! gitcommit * <buffer>'
