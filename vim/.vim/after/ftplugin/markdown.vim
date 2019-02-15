" Markdown
if &filetype !=# 'markdown'
  finish
endif

setlocal textwidth=78

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= '|setl tw<'
else
  let b:undo_ftplugin = 'setl tw<'
endif
