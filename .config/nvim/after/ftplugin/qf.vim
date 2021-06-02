let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

nnoremap <silent> <buffer> q :q<CR>

let b:undo_ftplugin .= '|nun <buffer> q'

augroup qf
  autocmd!
  autocmd BufEnter <buffer> ++nested if winnr('$') < 2 | q | endif
augroup END
let b:undo_ftplugin .= '|au! qf * <buffer>'

if !exists('loaded_cfilter')
  silent! packadd cfilter

  " Set this variable regardless of whether or not the plugin was successfully
  " loaded so that we don't try to load it again
  let loaded_cfilter = get(g:, 'loaded_cfilter')
endif
