" Python specific settings
if &filetype !=# 'python'
  finish
endif

" If a valid formatter executable is found, autoformat the buffer when writing
" the file
let g:python_format_on_write = 1

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

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')
let b:undo_ftplugin .= '|setl path< cpt< cc<'

if executable('black')
  setl formatprg=black\ -q\ -
elseif executable('yapf')
  setl formatprg=yapf
endif

if !empty(&l:formatprg)
  augroup python.vim.PreWrite
    autocmd!
    autocmd BufWritePre <buffer>
          \ if g:python_format_on_write |
          \   let view = winsaveview() |
          \   execute '%!' . &l:formatprg |
          \   call winrestview(view) |
          \   unlet view |
          \ endif
  augroup END
  let b:undo_ftplugin .= ' fp<|au! python.vim.PreWrite'
endif
