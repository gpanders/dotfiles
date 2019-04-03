" Vimwiki
setlocal textwidth=80
setlocal spell
setlocal complete+=k

let b:undo_ftplugin = 'setl tw< spell< cpt<'

" Replace default mappings
silent! nunmap <buffer> =
silent! nunmap <buffer> -
silent! nunmap <buffer> gl\*
silent! nunmap <buffer> <leader>whh
nmap <buffer> == <Plug>VimwikiAddHeaderLevel
nmap <buffer> -- <Plug>VimwikiRemoveHeaderLevel
nnoremap <silent> <buffer> gl* :VimwikiChangeSymbolTo \*<CR>
nmap <buffer> <leader>wb <Plug>Vimwiki2HTMLBrowse
nmap <buffer> >> <Plug>VimwikiIncreaseLvlSingleItem
nmap <buffer> << <Plug>VimwikiDecreaseLvlSingleItem

" Add mapping to compile entire wiki into HTML
nnoremap <silent> <buffer> <leader>wH :VimwikiAll2HTML<CR>

let b:undo_ftplugin = b:undo_ftplugin
      \ . '|nun <buffer> =='
      \ . '|nun <buffer> --'
      \ . '|nun <buffer> gl*'
      \ . '|nun <buffer> <leader>wb'
      \ . '|nun <buffer> >>'
      \ . '|nun <buffer> <<'
      \ . '|nun <buffer> <leader>wH'

if executable('dict')
  setlocal keywordprg=dict
  let b:undo_ftplugin .= '|setl kp<'
endif
