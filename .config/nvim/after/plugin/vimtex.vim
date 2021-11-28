if executable('skim')
  let g:vimtex_view_method = 'skim'
endif

let g:vimtex_compiler_progname = 'nvr'
let g:vimtex_fold_enabled = 1
let g:vimtex_toc_config = {'refresh_always' : 0}
let g:vimtex_compiler_latexmk_engines = { '_' : '-lualatex' }

augroup plugin_vimtex
  autocmd!
  autocmd BufRead *.tex ++once if get(g:, 'loaded_vimtex')
        \ | exe 'autocmd plugin_vimtex BufWritePost *.tex call vimtex#toc#refresh()'
        \ | endif
augroup END
