" Python specific settings
if &filetype !=# 'python'
  finish
endif

setlocal keywordprg=pydoc

" gz opens a split window with a python shell
nmap <buffer> gz <Plug>(PytermOpen)
