" vim-easy-align configuration
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-02-28

if !get(g:, 'loaded_easy_align_plugin', 0)
  finish
endif

let s:save_cpo = &cpo
set cpo&vim

nmap gl <Plug>(EasyAlign)
xmap gl <Plug>(EasyAlign)

" Source: https://gist.github.com/tpope/287147
inoremap <silent> <Bar>   <Bar><Esc>:call <SID>align()<CR>a

function! s:align()
  let p = '^\s*|\s.*\s|\s*$'
  let l = line('.')
  if getline('.') =~# '^\s*|' && (getline(l-1) =~# p || getline(l+1) =~# p)
    let column = strlen(substitute(getline('.')[0:col('.')],'[^|]','','g'))
    let position = strlen(matchstr(getline('.')[0:col('.')],'.*|\s*\zs.*'))
    normal glip*|
    call cursor(l, 0)
    call search(repeat('[^|]*|', column) . '\s\{-\}' . repeat('.', position), 'ce', l)
  endif
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo
