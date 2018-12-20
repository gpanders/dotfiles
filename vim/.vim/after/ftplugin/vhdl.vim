if &filetype !=# 'vhdl'
  finish
endif

setl commentstring=--%s

let b:undo_ftplugin = 'setl cms<'
