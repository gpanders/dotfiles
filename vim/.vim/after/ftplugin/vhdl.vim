" VHDL filetype plugin
" Author: Greg Anders <greg@gpanders.com>

setlocal commentstring=--%s
setlocal tagcase=ignore

" VHDL is case insensitive
setlocal ignorecase
setlocal nosmartcase

if executable('ghdl')
  compiler ghdl
endif

let b:undo_ftplugin = 'setl cms< tc< ic< scs<'
