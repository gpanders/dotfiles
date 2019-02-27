" Python specific settings
if &filetype !=# 'python'
  finish
endif

" gz opens a split window with a python shell
nmap <buffer> gz <Plug>(PytermOpen)

" Don't complete from include files (there are too many!)
setlocal complete-=i

" Show boundary at 80 characters
let &l:colorcolumn = join(range(80, 336), ',')

" Populate path from python's sys.path. This is an expensive operation so we
" only call it once and then cache the result
if !exists('g:python_include_path')
  let g:python_include_path = python#include_path()
endif

let &l:path = &path . ',' . g:python_include_path

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= '|setl path< cpt< cc<'
else
  let b:undo_ftplugin = '|setl path< cpt< cc<'
endif

if executable('yapf')
  setl formatprg=yapf
  let b:undo_ftplugin .= ' fp<'
endif
