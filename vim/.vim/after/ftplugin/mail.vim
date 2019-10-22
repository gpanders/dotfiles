" mail filetype configuration
" Author: Greg Anders
" Date: 2018-12-02

setlocal formatoptions+=wa
setlocal wrapmargin=0
setlocal nonumber
setlocal nolist
setlocal spell
let b:undo_ftplugin .= '|setl fo< wm< nu< list< spell<'

let b:highlight_trailing_whitespace = 0
let b:undo_ftplugin .= '|unlet! b:highlight_trailing_whitespace'
