let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

" black uses 88 chars by default
setlocal textwidth=88

" Don't auto-wrap lines unless they're comments
setlocal formatoptions-=t

setlocal foldmethod=indent
setlocal foldnestmax=2

let b:undo_ftplugin .= '|setl tw< fo< fdm< fdn<'

" Make [d, [i, etc. find variables defined at the top-level (no indent)
setlocal define=^\\ze\\i\\+\\s*=

if exists('$VIRTUAL_ENV') && filereadable($VIRTUAL_ENV . '/tags')
    let &l:tags = $VIRTUAL_ENV . '/tags,' . &tags
endif

if executable('black')
    setlocal formatprg=black\ -q\ -\ 2>/dev/null
elseif executable('yapf')
    setlocal formatprg=yapf
endif

if executable('isort')
    if empty(&l:formatprg)
        setlocal formatprg=isort\ -q\ -
    else
        setlocal formatprg+=\ \|\ isort\ -q\ -
    endif
endif

if !empty(&l:formatprg)
    let b:undo_ftplugin .= '|setl fp<'
endif
