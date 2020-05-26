let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

" enable spell check
setlocal spell
setlocal textwidth=72
setlocal formatoptions+=cn
setlocal formatlistpat=^\\s*\\d\\+\\.\\s\\+\\\|^\\s*[-*+]\\s\\+\\\|^\\[^\\ze[^\\]]\\+\\]:

let b:undo_ftplugin .= '|setl tw< spell< fo< flp<'

" warning if first line too long
match ErrorMsg /\%1l.\%>51v/
