if !get(g:, 'loaded_fugitive')
  finish
endif

augroup plugin.fugitive
    autocmd!
    autocmd BufReadPost fugitive://* setlocal bufhidden=delete
    autocmd User Fugitive setlocal grepprg=git\ grep\ --line-number\ --column\ -I\ --untracked
    autocmd FileType fugitive,fugitiveblame nmap <silent> <buffer> q gq
augroup END
