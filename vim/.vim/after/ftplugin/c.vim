" C filetype plugin
" Author: Greg Anders <greg@gpanders.com>

" Set comment string
setlocal commentstring=//%s

" Set include pattern
setlocal include=^\\s*#\\s*include

" Keep lines at 80 characters or fewer
setlocal textwidth=79

" Support /// as a comment leader, used for writing Doxygen comments
setlocal comments-=://
setlocal comments+=:///,://

" Include headers on Unix
if has('unix')
  setlocal path=.,/usr/local/include,/usr/include,,
endif

let b:undo_ftplugin .= '|setl cms< inc< path< tw< com<'

" Make [[, ]], etc. work even when {'s are not in the first column
noremap <buffer> [[ ?{<CR>w99[{
noremap <buffer> ][ /}<CR>b99]}
noremap <buffer> ]] j0[[%/{<CR>
noremap <buffer> [] k$][%?}<CR>

let b:undo_ftplugin = b:undo_ftplugin
      \ . '|nun <buffer> [['
      \ . '|nun <buffer> ]['
      \ . '|nun <buffer> ]]'
      \ . '|nun <buffer> []'

" Use improved :Man command as keywordprg
if exists(':Man') == 2
  setlocal keywordprg=:Man
  let b:undo_ftplugin .= '|setl kp<'
endif

if executable('clang-format')
  setlocal formatprg=clang-format\ -style=file\ -fallback-style=LLVM
  let b:undo_ftplugin .= '|setl fp<'
endif

