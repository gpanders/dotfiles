" Python specific settings
let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

" gz opens a split window with a python shell
if !exists('g:pyterm_cmd')
  for s:cmd in ['ipython', 'python3', 'python']
    if executable(s:cmd)
      let g:pyterm_cmd = s:cmd
      break
    endif
  endfor
  unlet s:cmd
endif

if exists('g:pyterm_cmd')
  nnoremap <silent> <buffer> gz :<C-U>call easyterm#open(g:pyterm_cmd)<CR>
  let b:undo_ftplugin .= '|nun <buffer> gz'
endif

" Set textwidth to 88 to mimic black
setlocal textwidth=88

" Set format options
setlocal formatoptions-=t " Don't auto-wrap lines unless they're comments

" Use indent for foldmethod
setlocal foldmethod=indent
setlocal foldnestmax=2

let b:undo_ftplugin .= '|setl tw< fo< fdm< fdn<'

" Make [d, [i, etc. find variables defined at the top-level (no indent)
setlocal define=^\\ze\\i\\+\\s*=

" Try to infer python version from shebang
let s:python = matchstr(getline(1), '^#!.*\zspython\([23]\)\?')

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
