if executable('jq')
  setlocal formatprg=jq
  let b:undo_ftplugin = get(b:, 'undo_ftplugin', '') . '|setl fp<'
endif
