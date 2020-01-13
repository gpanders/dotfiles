let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

if executable('prettier')
  setlocal formatprg=prettier\ --parser\ css
endif

if !empty(&l:formatprg)
  let b:undo_ftplugin .= '|setl fp<'
endif
