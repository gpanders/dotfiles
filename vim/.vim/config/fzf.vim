if exists('plugs') && has_key(plugs, 'fzf.vim')
  let g:fzf_tags_command = 'ctags -R'

  map <silent> <C-P> :Files<CR>

  if executable('rg')
    nnoremap <C-\>k :Rg<space>
  elseif executable('ag')
    nnoremap <C-\>k :Ag<space>
  end
  nnoremap <leader>b :Buffers<CR>

  function! s:fzf_statusline()
    set laststatus=0
    autocmd BufWinLeave <buffer> set laststatus=2
  endfunction

  autocmd! User FzfStatusLine call <SID>fzf_statusline()
endif
