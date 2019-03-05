" mail filetype configuration
" Author: Greg Anders
" Date: 2018-12-02

if &filetype !=# 'mail'
  finish
endif

setlocal formatoptions+=wa
setlocal wrapmargin=0
setlocal nonumber
setlocal digraph
setlocal nolist

let b:undo_ftplugin .= '|setl fo< wm< nu< dg< list<'
