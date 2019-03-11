" FZF must be installed to ~/.fzf
if !get(g:, 'loaded_fzf', 0) || exists(':Files') != 2
  finish
endif

nnoremap <silent> ,f :Files<CR>
nnoremap <silent> ,t :Tags<CR>
nnoremap <silent> ,b :Buffers<CR>
nnoremap <silent> <Space>m :Marks<CR>

" Emacs-like M-x command
nnoremap <silent> <Space><Space> :Commands<CR>

autocmd! FileType fzf
autocmd FileType fzf set laststatus=0 noruler
      \| autocmd BufLeave <buffer> set laststatus=2 ruler
