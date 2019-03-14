" fzf configuration
" Author: Greg Anders <greg@gpanders.com>

if !get(g:, 'loaded_fzf', 0)
  finish
endif

nnoremap <silent> ,f :FZF<CR>

autocmd! FileType fzf set laststatus=0 noruler
      \ | autocmd BufLeave <buffer> set laststatus=2 ruler
