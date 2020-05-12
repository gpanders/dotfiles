let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

if executable('zig')
    setlocal formatprg=zig\ fmt\ --stdin
    let b:undo_ftplugin .= '|setl fp<'
endif
