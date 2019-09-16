" vim-dispatch configuration
" Author: Greg Anders <greg@gpanders.com>
" Date: 2018-12-03

if !get(g:, 'loaded_dispatch')
  finish
endif

if !exists('g:dispatch_compilers')
  let g:dispatch_compilers = {}
endif

" The pytest compiler uses `py.test' as its command, so Dispatch won't find it
" when the command is `pytest' (without the dot) unless we add this
let g:dispatch_compilers['pytest'] = 'pytest'

augroup plugin.dispatch
  autocmd!

  " Python
  autocmd FileType python let b:dispatch = 'pytest --tb=short -q'

  " TeX
  autocmd FileType tex let b:dispatch = 'chktex -q -v0 -- %:S'

  " Bash
  autocmd FileType sh let b:dispatch = 'shellcheck -f gcc -- %:S'
augroup END
