" vim-fugitive configuration
" Author: Greg Anders <greg@gpanders.com>
" Date: 2018-12-03

if !get(g:, 'loaded_fugitive')
  finish
endif

augroup plugin.fugitive
    autocmd!
    autocmd BufReadPost fugitive://* setlocal bufhidden=delete
augroup END
