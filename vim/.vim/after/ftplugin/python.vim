" Python specific settings
if &filetype !=# 'python'
  finish
endif

setlocal complete+=i

" gz opens a split window with a python shell
nmap <buffer> gz <Plug>(PytermOpen)

if !exists('g:python_include_path')
  let g:python_include_path = python#include_path()
endif

let &l:path = &path . g:python_include_path

let b:undo_ftplugin .= '|setl cpt<'
