" Markdown
if &filetype !=# 'markdown'
  finish
endif

setlocal textwidth=80
setlocal spell

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= '|setl tw< spell<'
else
  let b:undo_ftplugin = 'setl tw< spell<'
endif
