setlocal noexpandtab
setlocal shiftwidth=0

let b:undo_indent = get(b:, 'undo_indent', '') . '|setl et< sw<'
