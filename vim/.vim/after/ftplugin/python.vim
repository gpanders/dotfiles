" Python specific settings
let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

" gz opens a split window with a python shell
nmap <buffer> gz <Plug>(PytermOpen)

" Set textwidth to 88 to mimic black
setlocal textwidth=88

" Set format options
setlocal formatoptions-=t " Don't auto-wrap lines unless they're comments

" Use indent for foldmethod
setlocal foldmethod=indent
setlocal foldnestmax=2

let b:undo_ftplugin .= '|setl tw< fo< fdm< fdn<'

" Try to infer python version from shebang
let s:line1 = getline(1)
let s:python = matchstr(s:line1, 'python\([23]\)\?')

" Populate path from python's sys.path
call ft#python#set_path(s:python)
let b:undo_ftplugin .= '|setl path<'

" Use a wrapper around the default !pydoc command for keywordprg
command! -buffer -nargs=? Pydoc call ft#python#pydoc(<q-args>)
setlocal keywordprg=:Pydoc
let b:undo_ftplugin .= '|delc Pydoc|setl kp<'

if executable('black')
  setlocal formatprg=black\ -q\ -\ 2>/dev/null
elseif executable('yapf')
  setlocal formatprg=yapf
endif

if !empty(&l:formatprg)
  let b:undo_ftplugin .= '|setl fp<'
endif
