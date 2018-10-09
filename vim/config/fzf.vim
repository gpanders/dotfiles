if exists('plugs') && has_key(plugs, 'fzf.vim')
  let g:fzf_tags_command = 'ctags -R'

  map <silent> <C-P> :Files<CR>
  map <silent> <C-E> :Tags<CR>
  map <silent> <C-Y> :BTags<CR>
  map <silent> <C-B> :Buffers<CR>

  function! s:fzf_statusline()
    set laststatus=0
    autocmd BufWinLeave <buffer> set laststatus=2
  endfunction

  autocmd! User FzfStatusLine call <SID>fzf_statusline()
endif
