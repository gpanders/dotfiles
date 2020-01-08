" yaml filetype plugin
" Author: Greg Anders <greg@gpanders.com>

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

setlocal textwidth=80

let b:undo_ftplugin .= '|setl tw<'

if executable('prettier')
  setlocal formatprg=prettier\ --parser\ yaml
  let b:undo_ftplugin .= ' fp<'
endif
