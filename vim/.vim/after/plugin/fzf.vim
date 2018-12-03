if exists('plugs') && has_key(plugs, 'fzf.vim')
  let g:fzf_tags_command = 'ctags -R'

  map <silent> <C-P> :Files<CR>

  " nnoremap <leader>t :Tags<CR>
  " nnoremap <leader>b :Buffers<CR>

  autocmd! FileType fzf
  autocmd FileType fzf set laststatus=0 noshowmode noruler
        \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler
endif
