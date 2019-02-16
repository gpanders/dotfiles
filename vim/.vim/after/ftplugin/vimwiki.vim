" Vimwiki
if &filetype !=# 'vimwiki'
  finish
endif

setlocal textwidth=80
setlocal spell
setlocal complete+=k

let b:undo_ftplugin = 'setl tw< spell< kp< cpt<'

if executable('dict')
  setlocal keywordprg=dict
  let b:undo_ftplugin .= '|setl kp<'
endif
