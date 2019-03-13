" Markdown
if &filetype !=# 'markdown'
  finish
endif

compiler pandoc

setlocal textwidth=80
setlocal spell

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= '|setl tw< spell<'
else
  let b:undo_ftplugin = 'setl tw< spell<'
endif

if exists(':Goyo') == 2
  Goyo
  let b:undo_ftplugin .= '|Goyo!'
endif
