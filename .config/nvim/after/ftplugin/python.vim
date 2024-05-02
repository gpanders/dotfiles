if executable('ruff')
    let &l:formatprg = printf('ruff check -s --select I --fix-only --stdin-filename %1$s - | ruff format --stdin-filename %1$s -', expand('%:p'))
elseif executable('black')
    let &l:formatprg = 'black -q - 2>/dev/null'
    if executable('isort')
        if empty(&l:formatprg)
            let &l:formatprg = 'isort -q -'
        else
            let &l:formatprg .= ' | isort -q - '
        endif
    endif
endif

if !empty(&l:formatprg)
    let b:undo_ftplugin .= '|setl fp<'
endif
