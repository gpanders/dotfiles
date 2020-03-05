setlocal tabstop=99
setlocal shiftwidth=0
setlocal softtabstop=0

let b:undo_indent = get(b:, 'undo_indent', ''). '|setl ts< sw< sts<'
