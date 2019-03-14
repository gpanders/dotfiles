" C filetype plugin
" Author: Greg Anders <greg@gpanders.com>

if &filetype !=# 'c' && &filetype !=# 'cpp'
  finish
endif

" Set comment string
setlocal commentstring=//%s

" Include macros in completion
setlocal complete+=i,d

" Set include pattern
setlocal include=^\\s*#\\s*include

" Include headers on Unix
if has('unix')
  setlocal path=.,/usr/local/include,/usr/include,,
endif

" Make [[, ]], etc. work even when {'s are not in the first column
noremap [[ ?{<CR>w99[{
noremap ][ /}<CR>b99]}
noremap ]] j0[[%/{<CR>
noremap [] k$][%?}<CR>

let b:undo_ftplugin .= '|setl cms< cpt< inc< path<'

" Use improved :Man command as keywordprg
if exists(':Man') == 2
  setlocal keywordprg=:Man
  let b:undo_ftplugin .= ' kp<'
endif

if executable('clang-format')
  setlocal formatprg=clang-format\ -style=file\ -fallback-style=Google
  let b:undo_ftplugin .= ' fp<'
endif

