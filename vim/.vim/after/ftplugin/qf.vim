let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

nnoremap <silent> <buffer> q :q<CR>

let b:undo_ftplugin .= '|nun <buffer> q'

if exists(':Cfilter') != 2
  silent packadd cfilter
endif
