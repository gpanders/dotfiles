" vim-projectionist configuration
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-01-04

if !get(g:, 'loaded_projectionist', 0)
  finish
endif

if !exists('g:projectionist_transformations')
  let g:projectionist_transformations = {}
endif

function! g:projectionist_transformations.date(input, o) abort
  return strftime('%Y-%m-%d')
endfunction

function! s:activate()
  " Binary (on/off) settings
  for opt in ['textwidth', 'shiftwidth', 'softtabstop', 'expandtab']
    for [root, value] in projectionist#query(opt)
      execute 'setlocal ' . opt . '=' . value
    endfor
  endfor

  " List settings
  for opt in ['suffixes', 'wildignore']
    for [root, value] in projectionist#query(opt)
      if type(value) ==# type([])
        let value = join(value, ',')
      endif
      execute 'setlocal ' . opt . '^=' . value
    endfor
  endfor
endfunction

autocmd User ProjectionistActivate call s:activate()
