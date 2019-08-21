" cscope
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-08-05

if !has('cscope') || !executable('cscope') || exists('g:loaded_cscope')
  finish
endif
let g:loaded_cscope = 1

if filereadable(expand($CSCOPE_DB))
  set cscoperelative
  silent cscope add $CSCOPE_DB
elseif filereadable('cscope.out')
  silent cscope add cscope.out
else
  finish
endif
set cscopetag
set cscopepathcomp=2

" See :h cscope-find for details
nnoremap <C-_>s :cscope find s <C-R>=expand('<cword>')<CR><CR>
nnoremap <C-_>g :cscope find g <C-R>=expand('<cword>')<CR><CR>
nnoremap <C-_>c :cscope find c <C-R>=expand('<cword>')<CR><CR>
nnoremap <C-_>t :cscope find t <C-R>=expand('<cword>')<CR><CR>
nnoremap <C-_>e :cscope find e <C-R>=expand('<cword>')<CR><CR>
nnoremap <C-_>f :cscope find f <C-R>=expand('<cfile>')<CR><CR>
nnoremap <C-_>i :cscope find i <C-R>=expand('<cfile>')<CR><CR>
nnoremap <C-_>d :cscope find d <C-R>=expand('<cword>')<CR><CR>
nnoremap <C-_>a :cscope find a <C-R>=expand('<cword>')<CR><CR>
