let b:undo_indent = get(b:, 'undo_indent', '')

setlocal noexpandtab
setlocal shiftwidth&
setlocal softtabstop&

let b:undo_indent .= '|setl et< sw< sts<'
