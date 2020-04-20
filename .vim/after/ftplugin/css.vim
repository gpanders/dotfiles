if executable('prettier')
  setlocal formatprg=prettier\ --parser\ css\ --config-precedence\ prefer-file
  let b:undo_ftplugin = get(b:, 'undo_ftplugin', '') . '|setl fp<'
endif
