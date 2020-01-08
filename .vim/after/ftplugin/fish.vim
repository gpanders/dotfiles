" fish filetype plugin
" Author: Greg Anders <greg@gpanders.com>

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

if executable('fish_indent')
  setlocal formatprg=fish_indent
  let b:undo_ftplugin .= '|setl fp<'
endif
