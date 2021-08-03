let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

setlocal commentstring=;\ %s

let b:undo_ftplugin .= '|setl< cms'
