" fzf configuration
" Author: Greg Anders <greg@gpanders.com>

if !get(g:, 'loaded_fzf', 0)
  finish
endif

" Ctrl-P to start FZF
nnoremap <silent> <C-P> :FZF<CR>

" Create Helptags command and map it to Ctrl-H
command! -bang Helptags call fzf#helptags(<bang>0)
nnoremap <silent> <C-H> :<C-U>Helptags<CR>

" Hide statusline in FZF buffers
autocmd! FileType fzf set laststatus=0 noruler
      \ | autocmd BufLeave <buffer> set laststatus=2 ruler
