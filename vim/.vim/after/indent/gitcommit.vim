" Git commit indent configuration
if &filetype !=# 'gitcommit'
  finish
endif

setlocal shiftwidth=2
setlocal softtabstop=2

let b:undo_indent = 'setl sw< sts<'
