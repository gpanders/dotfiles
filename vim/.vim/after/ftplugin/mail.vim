setlocal formatoptions+=wa
setlocal wrapmargin=0
setlocal nonumber
setlocal digraph
setlocal nolist

let b:undo_ftplugin .= '|setlocal fo< wm< nu< dg< list<'
