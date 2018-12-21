if !exists('g:loaded_dispatch')
  finish
endif

augroup Dispatch
  autocmd!

  " C/C++
  autocmd FileType c,cpp let b:dispatch = 'cppcheck %:S'

  " " Python
  autocmd FileType python let b:start = 'ipython'
                      \ | let b:dispatch = 'pylint -f parseable -s no -- %:S'
augroup END
