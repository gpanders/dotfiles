let b:undo_indent = get(b:, 'undo_indent', '')

setlocal noexpandtab
setlocal shiftwidth&vim
setlocal softtabstop&vim
let b:undo_ftplugin .= '|setl et< sw< sts<'
