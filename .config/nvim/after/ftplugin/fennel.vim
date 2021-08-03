let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

setlocal commentstring=;%s
setlocal comments=:;

let b:undo_ftplugin .= '|setl cms< com<'
