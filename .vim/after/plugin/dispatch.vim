if !get(g:, 'loaded_dispatch')
  finish
endif

if !exists('g:dispatch_compilers')
  let g:dispatch_compilers = {}
endif

" The pytest compiler uses `py.test' as its command, so Dispatch won't find it
" when the command is `pytest' (without the dot) unless we add this
let g:dispatch_compilers['pytest'] = 'pytest'

function! s:set_dispatch(val)
    if !exists('b:dispatch')
        let b:dispatch = a:val
    endif
endfunction

augroup plugin.dispatch
  autocmd!

  " Python
  autocmd FileType python call s:set_dispatch('pytest --tb=short -q')

  " TeX
  autocmd FileType tex call s:set_dispatch('chktex -q -v0 -- %:S')

  " Bash
  autocmd FileType sh call s:set_dispatch('shellcheck -f gcc -- %:S')
augroup END
