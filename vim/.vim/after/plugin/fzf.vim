" FZF must be installed to ~/.fzf
if !get(g:, 'loaded_fzf', 0)
  finish
endif

nnoremap <silent> <C-P> :Files<CR>
nnoremap <silent> ,t :Tags<CR>
nnoremap <silent> ,k :Marks<CR>
nnoremap <silent> ,b :Buffers<CR>

" Emacs-like M-x command
nnoremap <silent> <Space><Space> :Commands<CR>

autocmd! FileType fzf
autocmd FileType fzf set laststatus=0 noruler
      \| autocmd BufLeave <buffer> set laststatus=2 ruler
