command! -nargs=* -complete=buffer B call quick_buffer#QuickBuffer(<q-args>)

nnoremap <leader>B :B<CR>
