" Python specific settings
let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

" gz opens a split window with a python shell
nmap <buffer> gz <Plug>(PytermOpen)

" Don't complete from include files (there are too many!)
setlocal complete-=i

" Set textwidth to 88 to mimic black
setlocal textwidth=88

" Set format options
setlocal formatoptions-=t " Don't auto-wrap lines unless they're comments

" Use flake8 as makeprg for linting
compiler flake8

" Populate path from python's sys.path
call python#set_path()

let b:undo_ftplugin .= '|setl path< cpt< tw< fo<'

if executable('black')
  setlocal formatprg=black\ -q\ -
elseif executable('yapf')
  setlocal formatprg=yapf
endif

if !empty(&l:formatprg)
  let b:undo_ftplugin .= '|setl fp<'
endif
