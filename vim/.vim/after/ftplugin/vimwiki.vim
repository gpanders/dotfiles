" Vimwiki
if &filetype !=# 'vimwiki'
  finish
endif

setlocal textwidth=80
setlocal spell
setlocal complete+=k

let b:undo_ftplugin = 'setl tw< spell< kp< cpt<'

" Replace default mappings
silent! nunmap <buffer> =
silent! nunmap <buffer> -
nmap <buffer> == <Plug>VimwikiAddHeaderLevel
nmap <buffer> -- <Plug>VimwikiRemoveHeaderLevel

let b:undo_ftplugin .= '|nun <buffer> ==|nun <buffer> --'

if executable('dict')
  setlocal keywordprg=dict
  let b:undo_ftplugin .= '|setl kp<'
endif
