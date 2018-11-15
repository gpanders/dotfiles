if exists('plugs') && has_key(plugs, 'fzf.vim')
  let g:fzf_tags_command = 'ctags -R'

  map <silent> <C-P> :Files<CR>

  nnoremap <C-\>k :Ag<space>
  nnoremap <leader>b :Buffers<CR>

  function! s:fzf_statusline()
    set laststatus=0
    autocmd BufWinLeave <buffer> set laststatus=2
  endfunction

  autocmd! User FzfStatusLine call <SID>fzf_statusline()
endif
