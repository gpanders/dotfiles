if executable('prettier')
    setlocal formatprg=prettier\ --use-tabs\ --config-precedence\ prefer-file\ %
    let b:undo_ftplugin = get(b:, 'undo_ftplugin', '') . '|setl fp<'
endif
