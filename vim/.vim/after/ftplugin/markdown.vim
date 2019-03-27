" Markdown filetype plugin
" Author: Greg Anders <greg@gpanders.com>

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

if has('conceal')
  setlocal conceallevel=2
  let b:undo_ftplugin = 'setl cole<'
endif
