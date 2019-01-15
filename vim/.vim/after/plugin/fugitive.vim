if !exists('g:loaded_fugitive')
  finish
endif

autocmd BufReadPost fugitive://* setlocal bufhidden=delete
autocmd User Fugitive noremap ,g :Gstatus<CR>
