" coc.nvim configuration
" Author: Greg Anders

if !exists('g:did_coc_loaded') || !has('nvim')
  finish
endif

let g:coc_filetypes = ['c', 'cpp', 'rust']

autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif
inoremap <silent> <expr> <C-Space> coc#refresh()

augroup CocSetup
  autocmd!
  autocmd FileType * if index(g:coc_filetypes, &ft) >= 0 |
        \ setlocal formatexpr=CocAction('formatSelected') |
        \ exe "nmap <silent> <buffer> gr <Plug>(coc-references)" |
        \ exe "nmap <silent> <buffer> gd <Plug>(coc-definition)" |
        \ exe "au CursorHoldI,CursorMovedI <buffer> call CocAction('showSignatureHelp')" |
        \ endif
augroup END
