" FZF must be installed to ~/.fzf
if !exists('g:loaded_fzf')
  finish
endif

nnoremap <silent> <C-P> :Files<CR>
nnoremap <silent> ,t :Tags<CR>
nnoremap <silent> ,k :Marks<CR>
nnoremap <silent> ,b :Buffers<CR>

autocmd! FileType fzf
autocmd FileType fzf set laststatus=0 noruler
      \| autocmd BufLeave <buffer> set laststatus=2 ruler
