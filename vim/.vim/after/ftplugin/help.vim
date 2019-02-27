if &filetype !=# 'help'
  finish
endif

noremap <silent> <buffer> <CR> g<C-]>
noremap <silent> <C-J> /<Bar>\S\{-}<Bar><CR>:nohlsearch<CR>
noremap <silent> <C-K> ?<Bar>\S\{-}<Bar><CR>:nohlsearch<CR>

" Make help in vim more like a pager
noremap <silent> <nowait> <buffer> q :q<CR>
noremap <silent> <nowait> <buffer> d <C-d>
noremap <silent> <nowait> <buffer> u <C-u>
