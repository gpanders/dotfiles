if !exists('g:loaded_vimtex')
  finish
endif

let g:vimtex_compiler_progname = 'nvr'
let g:vimtex_matchparen_enabled = 0

if executable('skim')
  let g:vimtex_view_method = 'skim'
endif

let g:vimtex_fold_enabled = 1

set fillchars=vert:\|,fold:\\

let g:vimtex_labels_refresh_always = 0
let g:vimtex_toc_refresh_always = 0

augroup vimtex
  autocmd!
  autocmd BufWritePost *.tex call vimtex#labels#refresh()
  autocmd BufWritePost *.tex call vimtex#toc#refresh()
augroup END
