setlocal shiftwidth=2

let b:undo_indent = get(b:, 'undo_indent', '') . '|setl sw<'
