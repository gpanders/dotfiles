if exists('plugs') && has_key(plugs, 'fzf.vim')
  let g:fzf_tags_command = 'ctags -R'

  map <silent> <C-P> :Files<CR>

  if executable('rg')
    nnoremap <C-K> :Rg <C-R><C-W>
  elseif executable('ag')
    nnoremap <C-K> :Ag <C-R><C-W>
  end
  nnoremap <leader>b :Buffers<CR>

  function! s:fzf_statusline()
    set laststatus=0
    autocmd BufWinLeave <buffer> set laststatus=2
  endfunction

  autocmd! User FzfStatusLine call <SID>fzf_statusline()
endif
