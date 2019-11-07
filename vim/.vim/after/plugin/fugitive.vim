" vim-fugitive configuration
" Author: Greg Anders <greg@gpanders.com>
" Date: 2018-12-03

if !get(g:, 'loaded_fugitive')
  finish
endif

augroup plugin.fugitive
    autocmd!
    autocmd BufReadPost fugitive://* setlocal bufhidden=delete
    autocmd User Fugitive nnoremap <buffer> g<CR> :Ggrep<Space>
    autocmd FileType fugitive nmap <silent> <buffer> q gq
augroup END
