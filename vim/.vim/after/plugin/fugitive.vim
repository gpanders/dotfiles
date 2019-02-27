if !exists('g:loaded_fugitive')
  finish
endif

autocmd BufReadPost fugitive://* setlocal bufhidden=delete
autocmd User Fugitive nnoremap <Bslash>g :Gstatus<CR>
