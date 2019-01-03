if !exists('g:loaded_qf')
  finish
endif

" Enable Ack.vim style mappings in the quickfix window
let g:qf_mapping_ack_style = 1

" Don't auto open quickfix list because it screws up vim-dispatch
" https://github.com/tpope/vim-dispatch/issues/254
let g:qf_auto_open_quickfix = 0

" Redefine mappings from vim-unimpaired
nmap [q <Plug>(qf_qf_previous)
nmap ]q <Plug>(qf_qf_next)
nmap [l <Plug>(qf_loc_previous)
nmap ]l <Plug>(qf_loc_next)

nmap <C-Q> <Plug>(qf_qf_toggle)
