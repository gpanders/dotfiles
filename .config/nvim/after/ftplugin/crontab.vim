setlocal nobackup
setlocal nowritebackup

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '') . '|setl nobk< nowb<'
