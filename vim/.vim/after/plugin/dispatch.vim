if !exists('g:loaded_dispatch')
  finish
endif

augroup Dispatch
  autocmd!

  " C/C++
  autocmd FileType c,cpp let b:dispatch = 'clang-tidy -quiet %:S'

  " Python
  autocmd FileType python let b:dispatch = 'pylint -f parseable -s no -- %:S'

  " TeX
  autocmd FileType tex let b:dispatch = 'chktex -q -v0 -- %:S'

  " Bash
  autocmd FileType sh let b:dispatch = 'shellcheck -f gcc -- %:S'
augroup END
