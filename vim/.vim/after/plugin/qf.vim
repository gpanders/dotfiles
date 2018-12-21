if !exists('g:loaded_qf')
  finish
endif

" Enable Ack.vim style mappings in the quickfix window
let g:qf_mapping_ack_style = 1

" Redefine mappings from vim-unimpaired
nmap [q <Plug>(qf_qf_previous)
nmap ]q <Plug>(qf_qf_next)
nmap [l <Plug>(qf_loc_previous)
nmap ]l <Plug>(qf_loc_next)

nmap <C-Q> <Plug>(qf_qf_toggle)
