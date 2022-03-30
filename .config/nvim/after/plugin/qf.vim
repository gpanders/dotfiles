if !get(g:, 'loaded_qf')
  finish
endif

nmap ]q <Plug>(qf_qf_next)
nmap [q <Plug>(qf_qf_previous)
nmap ]l <Plug>(qf_loc_next)
nmap [l <Plug>(qf_loc_previous)
nmap <Space>q <Plug>(qf_qf_toggle_stay)
nmap <Space>l <Plug>(qf_loc_toggle_stay)

" Auto opening the quickfix list breaks vim-dispatch
let g:qf_auto_open_quickfix = 0
