" Python specific settings
let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

" If a valid formatter executable is found, autoformat the buffer when writing
" the file
let g:python_format_on_write = 0

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

" Populate path from python's sys.path. This is an expensive operation so we
" only call it once and then cache the result
if !exists('g:python_include_path')
  let g:python_include_path = python#include_path()
endif

let &l:path = &path . ',' . g:python_include_path

let b:undo_ftplugin .= '|setl path< cpt< tw< fo<'

if executable('black')
  setlocal formatprg=black\ -q\ -
elseif executable('yapf')
  setlocal formatprg=yapf
endif

if !empty(&l:formatprg)
  augroup ftplugin.python
    autocmd!
    autocmd BufWritePre <buffer>
          \ if g:python_format_on_write |
          \   let view = winsaveview() |
          \   execute '%!' . &l:formatprg |
          \   call winrestview(view) |
          \   unlet view |
          \ endif
  augroup END
  let b:undo_ftplugin .= ' fp<|exe "au! ftplugin.python * <buffer>"'
endif
