" help filetype plugin
" Author: Greg Anders <greg@gpanders.com>

if &filetype !=# 'help' || !&readonly
  finish
endif

noremap <silent> <buffer> <CR> g<C-]>
noremap <silent> <buffer> <C-J> /<Bar>\S\{-}<Bar><CR>:nohlsearch<CR>
noremap <silent> <buffer> <C-K> ?<Bar>\S\{-}<Bar><CR>:nohlsearch<CR>

" Make help in vim more like a pager
noremap <silent> <nowait> <buffer> q :q<CR>
noremap <silent> <nowait> <buffer> d <C-d>
noremap <silent> <nowait> <buffer> u <C-u>
