" crontab
if &filetype !=# 'crontab'
  finish
endif

setlocal nobackup
setlocal nowritebackup

let b:undo_ftplugin .= '|nobackup< nowritebackup<'
