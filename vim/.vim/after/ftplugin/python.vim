" Python specific settings
if &filetype !=# 'python'
  finish
endif

setlocal keywordprg=pydoc

" gz opens a split window one-third the height of the main window with a
" python shell
noremap <expr> <buffer> gz ":botright ".winheight(0)/3."sp \<Bar> term python\<CR>"
