if executable('jq')
    setlocal formatprg=jq
elseif executable('prettier')
    setlocal formatprg=prettier\ --parser\ json5\ --config-precedence\ prefer-file
endif

if !empty(&l:formatprg)
    let b:undo_ftplugin = get(b:, 'undo_ftplugin', '') . '|setl fp<'
endif
