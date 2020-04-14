let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

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

if exists('$VIRTUAL_ENV') && filereadable($VIRTUAL_ENV . '/tags')
    let &l:tags = $VIRTUAL_ENV . '/tags,' . &tags
endif

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

if executable('isort')
    function! s:isort(line1, line2)
        let view = winsaveview()
        exe a:line1 . ',' . a:line2 . '!isort -y -'
        call winrestview(view)
    endfunction
    command! -buffer -nargs=0 -range=% -bar Isort call <SID>isort(<line1>, <line2>)
    let b:undo_ftplugin .= '|delc -buffer Isort'
endif
