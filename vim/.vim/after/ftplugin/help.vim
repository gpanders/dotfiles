if &filetype !=# 'help'
  finish
endif

" Make help in vim more like a pager
noremap <silent> <nowait> <buffer> q :q<CR>
noremap <silent> <nowait> <buffer> d <C-d>
noremap <silent> <nowait> <buffer> u <C-u>
