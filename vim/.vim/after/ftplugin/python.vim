" Python specific settings
if &filetype !=# 'python'
  finish
endif

" gz opens a split window with a python shell
nmap <buffer> gz <Plug>(PytermOpen)

" Use pytest compiler by default
compiler pytest

if !exists('g:python_include_path')
  let g:python_include_path = python#include_path()
endif

let &l:path = &path . g:python_include_path
