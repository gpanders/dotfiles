" Python specific settings
if &filetype !=# 'python'
  finish
endif

" gz opens a split window with a python shell
nmap <buffer> gz <Plug>(PytermOpen)

if !exists('g:python_include_path')
  let g:python_include_path = python#include_path()
endif

let &l:path = &path . g:python_include_path

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= '|setl cpt<'
else
  let b:undo_ftplugin = '|setl cpt<'
endif

if executable('yapf')
  setl equalprg=yapf
  let b:undo_ftplugin .= ' ep<'
endif
