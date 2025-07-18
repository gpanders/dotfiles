setlocal nowrap winfixbuf cursorlineopt=both

nnoremap <silent> <buffer> q :q<CR>

augroup qf
  autocmd!
  autocmd BufEnter <buffer> ++nested if winnr('$') < 2 | q | endif
augroup END

if !exists('loaded_cfilter')
  silent! packadd cfilter

  " Set this variable regardless of whether or not the plugin was successfully
  " loaded so that we don't try to load it again
  let loaded_cfilter = get(g:, 'loaded_cfilter')
endif

let b:undo_ftplugin .= '|setl wrap< wfb< culopt<|nun <buffer> q|au! qf * <buffer>'
