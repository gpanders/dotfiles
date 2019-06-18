" crontab filetype plugin
" Author: Greg Anders <greg@gpanders.com>

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

setlocal nobackup
setlocal nowritebackup

let b:undo_ftplugin .= '|setl nobk< nowb<'
