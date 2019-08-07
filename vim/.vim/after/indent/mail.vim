let b:undo_indent = get(b:, 'undo_indent', '')

setlocal noautoindent

let b:undo_indent .= '|setl ai<'
