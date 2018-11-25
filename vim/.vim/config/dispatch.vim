" C/C++
autocmd FileType c,cpp let b:dispatch = 'cppcheck %'

" Python
autocmd FileType python let b:start = 'python'
autocmd FileType python let b:dispatch = 'pylint -f parseable %'
