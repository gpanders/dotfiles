" Make read only files more pager-like
" Author: Greg Anders <greg@gpanders.com>
" Date: 2018-12-18

augroup readonly
  au!
  au BufReadPost * if &readonly
        \| setl nonumber
        \| noremap <buffer> <nowait> d <C-D>
        \| noremap <buffer> <nowait> u <C-U>
        \| noremap <buffer> <nowait> q :q<CR>
        \| endif
augroup END
