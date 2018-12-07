source ~/.fzf/plugin/fzf.vim

map <silent> <C-P> :Files<CR>
nnoremap <silent> <leader>t :Tags<CR>

autocmd! FileType fzf
autocmd FileType fzf set laststatus=0 noshowmode noruler
      \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler
