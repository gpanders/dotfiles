" html ftplugin is also sourced for Markdown, so the following should only
" happen on actual HTML files
if &ft ==# 'html' && executable('prettier')
  setlocal formatprg=prettier\ --parser\ html
  let b:undo_ftplugin = get(b:, 'undo_ftplugin', '') . '|setl fp<'
endif
