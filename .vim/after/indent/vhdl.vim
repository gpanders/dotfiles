let g:vhdl_indent_genportmap = 0

" Use two spaces for tabs
setlocal shiftwidth=2

let b:undo_indent = get(b:, 'undo_indent', '') . '|setl sw<'
