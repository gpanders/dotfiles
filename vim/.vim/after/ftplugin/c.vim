" C
if &filetype !=# 'c'
  finish
endif

" Set comment string
setlocal commentstring=//%s

" Include macros in completion
setlocal complete+=d

" Set include pattern
setlocal include=^\\s*#\\s*include

" Include headers on Unix
if has('unix')
  setlocal path+=/usr/include
endif

if executable('clang-format')
  setlocal formatprg=clang-format
endif

let b:undo_ftplugin .= '|setlocal commentstring< complete< include< path<'
