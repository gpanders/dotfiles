let b:undo_indent = get(b:, 'undo_ftplugin')

setlocal softtabstop=2
setlocal shiftwidth=2

let b:undo_indent .= '|setl sts< sw<'
