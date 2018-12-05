setlocal formatoptions+=wa
setlocal wrapmargin=0
setlocal textwidth=78
setlocal nonumber
setlocal digraph
setlocal nolist

let b:undo_ftplugin .= '|setlocal fo< wm< tw< nu< dg< list<'
