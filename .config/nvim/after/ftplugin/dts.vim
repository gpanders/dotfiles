" Devicetree files use /include/ "file"
setlocal include=^\\s*\\%(\\/include\\/\\\|#\\s*include\\)

" Insert comment leader when pressing Return in insert mode or o in normal
" mode
setlocal formatoptions+=ro

let b:undo_ftplugin = get(b:, 'undo_ftplugin') . '|setl inc< fo<'
