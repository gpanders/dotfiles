if !exists('g:tabular_loaded')
  finish
endif

let s:save_cpo = &cpo
set cpo&vim

nnoremap ,a= :Tabularize /=<CR>
vnoremap ,a= :Tabularize /=<CR>
nnoremap ,a: :Tabularize /:<CR>
vnoremap ,a: :Tabularize /:<CR>

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
