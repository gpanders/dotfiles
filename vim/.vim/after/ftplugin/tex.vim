" LaTeX
if &filetype !=# 'tex'
  finish
endif

setlocal nocursorline
setlocal norelativenumber
" :NoMatchParen

setlocal equalprg=latexindent
