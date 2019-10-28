" scdoc filetype plugin
" Author: Greg Anders <greg@gpanders.com>

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

compiler scdoc

let b:undo_ftplugin .= '|setl mp< efm<'
