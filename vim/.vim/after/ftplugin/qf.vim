let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

nnoremap <silent> <buffer> q :q<CR>

let b:undo_ftplugin .= '|nun <buffer> q'

augroup qf
  autocmd!
  autocmd BufEnter <buffer> nested if winnr('$') < 2 | q | endif
augroup END
let b:undo_ftplugin .= '|au! qf * <buffer>'

if exists(':Cfilter') != 2
  silent packadd cfilter
endif
