let b:undo_indent = get(b:, 'undo_indent', '')

setlocal tabstop=99

let b:undo_indent .= '|setl ts<'
