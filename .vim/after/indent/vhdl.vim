let b:undo_indent = get(b:, 'undo_indent', '')

let g:vhdl_indent_genportmap = 0

" Use two spaces for tabs
setlocal softtabstop=2
setlocal shiftwidth=2

let b:undo_indent .= '|setl sts< sw<'
