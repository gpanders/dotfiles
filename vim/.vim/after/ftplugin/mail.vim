" mail filetype configuration
" Author: Greg Anders
" Date: 2018-12-02

setlocal formatoptions+=wa
setlocal wrapmargin=0
setlocal nonumber
setlocal digraph
setlocal nolist

syntax clear TrailingWhitespace

let b:undo_ftplugin .= '|setl fo< wm< nu< dg< list<'
