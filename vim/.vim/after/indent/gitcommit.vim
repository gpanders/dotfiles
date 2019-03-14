" Git commit indent configuration
" Author: Greg Anders <greg@gpanders.com>

if &filetype !=# 'gitcommit'
  finish
endif

let b:undo_indent = get(b:, 'undo_indent', '')

setlocal shiftwidth=2
setlocal softtabstop=2

let b:undo_indent .= '|setl sw< sts<'
