" mail filetype configuration
" Author: Greg Anders
" Date: 2018-12-02

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

setlocal formatoptions+=wan
setlocal formatlistpat=^\\s*\\d\\+\\.\\s\\+\\\|^\\s*[-*+]\\s\\+\\\|^\\[^\\ze[^\\]]\\+\\]:
setlocal wrapmargin=0
setlocal nonumber
setlocal nolist
setlocal spell

let b:undo_ftplugin .= '|setl fo< flp< wm< nu< list< spell<'
