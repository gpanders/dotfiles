" Zsh
if &filetype !=# 'zsh'
  finish
endif

" Z! execute line as shell command
" Inspired by the Shdo buffers from vim-dirvish
nnoremap <buffer> Z! ^"zyg_:!<C-R>z<CR>
