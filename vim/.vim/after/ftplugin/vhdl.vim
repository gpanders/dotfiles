if &filetype !=# 'vhdl'
  finish
endif

setl commentstring=--%s

if executable('ghdl')
  compiler ghdl
endif

let b:undo_ftplugin = 'setl cms<'
