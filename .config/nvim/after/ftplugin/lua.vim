let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

if executable('stylua')
  setlocal formatexpr=ft#lua#format()
  let b:undo_ftplugin .= '|setl fp<'
endif

setlocal textwidth=100
let b:undo_ftplugin .= '|setl tw<'
