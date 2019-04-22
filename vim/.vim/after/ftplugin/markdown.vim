" Markdown filetype plugin
" Author: Greg Anders <greg@gpanders.com>

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

compiler pandoc

setlocal textwidth=80
setlocal spell

" https://github.com/tpope/vim-markdown/issues/134
setlocal comments=n:>
setlocal formatlistpat=^\\s*\\d\\+\\.\\s\\+\\\|^\\s*[-*+]\\s\\+\\\|^\\[^\\ze[^\\]]\\+\\]:

let b:undo_ftplugin .= '|setl tw< spell<'

if has('conceal')
  setlocal conceallevel=2
  let b:undo_ftplugin .= '|setl cole<'
endif
