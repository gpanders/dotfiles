let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

if executable('stylua')
  setlocal formatprg=stylua\ -
  let b:undo_ftplugin .= '|setl fp<'
endif
