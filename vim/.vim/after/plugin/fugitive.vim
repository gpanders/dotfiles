" vim-fugitive configuration
" Author: Greg Anders <greg@gpanders.com>
" Date: 2018-12-03

if !get(g:, 'loaded_fugitive', 0)
  finish
endif

autocmd BufReadPost fugitive://* setlocal bufhidden=delete
