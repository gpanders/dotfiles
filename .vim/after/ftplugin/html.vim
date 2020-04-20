" html ftplugin is also sourced for Markdown, so the following should only
" happen on actual HTML files
if &filetype ==# 'html' && executable('prettier')
  setlocal formatprg=prettier\ --parser\ html\ --config-precedence\ prefer-file
  let b:undo_ftplugin = get(b:, 'undo_ftplugin', '') . '|setl fp<'
endif
