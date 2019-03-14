" cpp indent configuration
" Author: Greg Anders <greg@gpanders.com>

if &filetype !=# 'cpp'
  finish
endif

let b:undo_indent = get(b:, 'undo_indent', '')

" Use two spaces for indenting per Google's C++ style guide
setlocal softtabstop=2
setlocal shiftwidth=2

let b:undo_indent .= '|setl sts< sw<'
