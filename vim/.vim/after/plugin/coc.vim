if !exists('g:did_coc_loaded')
  finish
endif

autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif
inoremap <silent> <expr> <C-Space> coc#refresh()

augroup CocSetup
  au!
  autocmd FileType c,cpp,python,rust
        \ setlocal formatexpr=CocAction('formatSelected') |
        \ exe "nmap <silent> <buffer> gd <Plug>(coc-definition)" |
        \ exe "nmap <silent> <buffer> K :\<C-U>call CocAction('doHover')\<CR>"
augroup END
