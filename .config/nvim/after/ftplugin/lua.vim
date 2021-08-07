let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

setlocal comments^=:---

let b:undo_ftplugin .= '|setl com<'

if executable('stylua')
  setlocal formatprg=stylua\ -
  let b:undo_ftplugin .= ' fp<'
endif
