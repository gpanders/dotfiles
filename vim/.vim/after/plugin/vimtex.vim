if !exists('g:loaded_vimtex')
  finish
endif

let g:tex_flavor = 'latex'
let g:matchup_matchparen_deferred = 1

if executable('skim')
  let g:vimtex_view_method = 'skim'
endif

let g:vimtex_fold_enabled = 1

set fillchars=vert:\|,fold:\\

augroup vimtex
  autocmd!
  autocmd BufWritePost *.tex call vimtex#labels#refresh()
  autocmd BufWritePost *.tex call vimtex#toc#refresh()
augroup END
