if executable('ruff')
    let &l:formatprg = 'ruff format --stdin-filename ' .. expand('%:p') .. ' -'
elseif executable('black')
    let &l:formatprg = 'black -q - 2>/dev/null'
endif

if executable('isort')
    if empty(&l:formatprg)
        let &l:formatprg = 'isort -q -'
    else
        let &l:formatprg .= ' | isort -q - '
    endif
endif

if !empty(&l:formatprg)
    let b:undo_ftplugin .= '|setl fp<'
endif
