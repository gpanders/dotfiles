" Toggle between light and dark background
if exists('g:loaded_toggle_bg') || !(has('termguicolors') || has('gui'))
  finish
endif
let g:loaded_toggle_bg = 1

command! -nargs=0 ToggleBg call toggle_bg#toggle()
