" coc.nvim configuration
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-01-04

if !get(g:, 'did_coc_loaded', 0) || !has('nvim')
  finish
endif

let g:coc_filetypes = ['c', 'cpp', 'rust']

augroup plugin.coc
  autocmd!
  autocmd FileType *
        \ if index(g:coc_filetypes, &ft) >= 0 |
        \   inoremap <buffer> <silent> <expr> <C-Space> coc#refresh() |
        \   nmap <silent> <buffer> gr <Plug>(coc-references) |
        \   nmap <silent> <buffer> gd <Plug>(coc-definition) |
        \   exec 'au! plugin.coc CursorHoldI,CursorMovedI <buffer> call CocAction("showSignatureHelp")' |
        \   let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')
        \     . '|setl fex<'
        \     . '|iun <buffer> <C-Space>'
        \     . '|nun <buffer> gr'
        \     . '|nun <buffer> gd'
        \     . '|exe "au! plugin.coc * <buffer>"' |
        \ endif
augroup END
