" tabular configuration
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-01-08

if !get(g:, 'tabular_loaded', 0)
  finish
endif

let s:save_cpo = &cpo
set cpo&vim

function! s:tabularize(dir)
  let char = nr2char(getchar())
  if a:dir ==# 'left'
    execute 'Tabularize/'.char.'/l1'
  else
    execute 'Tabularize/'.char.'/r1'
  endif
endfunction

noremap <silent> gl :<C-U>call <SID>tabularize('left')<CR>
noremap <silent> gL :<C-U>call <SID>tabularize('right')<CR>

" Source: https://gist.github.com/tpope/287147
inoremap <silent> <Bar>   <Bar><Esc>:call <SID>align()<CR>a

function! s:align()
  let p = '^\s*|\s.*\s|\s*$'
  if getline('.') =~# '^\s*|' && (getline(line('.')-1) =~# p || getline(line('.')+1) =~# p)
    let column = strlen(substitute(getline('.')[0:col('.')],'[^|]','','g'))
    let position = strlen(matchstr(getline('.')[0:col('.')],'.*|\s*\zs.*'))
    Tabularize/|/l1
    normal! 0
    call search(repeat('[^|]*|', column) . '\s\{-\}' . repeat('.', position), 'ce', line('.'))
  endif
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo
