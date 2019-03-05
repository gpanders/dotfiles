" yaml filetype plugin
" Author: Greg Anders <greg@gpanders.com>

if &filetype !=# 'yaml'
  finish
endif

setl textwidth=80

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')
let b:undo_ftplugin .= '|setl tw<'
