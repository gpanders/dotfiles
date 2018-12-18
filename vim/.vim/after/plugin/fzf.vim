" FZF must be installed to ~/.fzf
if !exists('g:loaded_fzf')
  finish
endif

nnoremap <silent> <C-P> :Files<CR>
nnoremap <silent> <leader>t :Tags<CR>
nnoremap <silent> <leader>k :Marks<CR>
nnoremap <silent> <leader>B :Buffers<CR>

autocmd! FileType fzf
autocmd FileType fzf set laststatus=0 noshowmode noruler
      \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler
