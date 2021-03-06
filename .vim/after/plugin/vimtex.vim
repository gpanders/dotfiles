if executable('skim')
  let g:vimtex_view_method = 'skim'
endif

let g:vimtex_compiler_progname = 'nvr'
let g:vimtex_fold_enabled = 1
let g:vimtex_toc_config = {'refresh_always' : 0}
let g:vimtex_compiler_latexmk_engines = { '_' : '-lualatex' }

augroup vimtex
  autocmd!
  autocmd BufWritePost *.tex call vimtex#toc#refresh()
augroup END
