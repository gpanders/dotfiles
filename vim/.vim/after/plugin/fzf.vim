" FZF must be installed to ~/.fzf
if !isdirectory($HOME . '/.fzf') || !exists(':FZF')
  finish
endif

source ~/.fzf/plugin/fzf.vim

nnoremap <silent> <C-P> :Files<CR>
nnoremap <silent> <leader>t :Tags<CR>
nnoremap <silent> <leader>B :Buffers<CR>

autocmd! FileType fzf
autocmd FileType fzf set laststatus=0 noshowmode noruler
      \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler
