" Python indent file

let b:undo_indent = get(b:, 'undo_indent', '')

setlocal shiftwidth=4
setlocal softtabstop=4

let b:undo_indent .= '|setl sw< sts<'
