" C filetype plugin
" Author: Greg Anders <greg@gpanders.com>

" Set comment string
setlocal commentstring=//%s

" Set include and define patterns
setlocal define=^\\s*#\\s*define
setlocal include=^\\s*#\\s*include

" Keep lines at 80 characters or fewer
setlocal textwidth=79

" Support /// as a comment leader, used for writing Doxygen comments
setlocal comments-=://
setlocal comments+=:///,://

" Include headers on Unix
if has('unix')
  setlocal path^=/usr/local/include,/usr/include
endif

" Ensure directory of current file is always first on the path
setlocal path-=.
setlocal path^=.

let b:undo_ftplugin .= '|setl cms< def< inc< path< tw< com<'

" Use improved :Man command as keywordprg
if exists(':Man') == 2
  setlocal keywordprg=:Man
  let b:undo_ftplugin .= '|setl kp<'
endif

if executable('clang-format')
  setlocal formatprg=clang-format\ -style=file\ -fallback-style=LLVM
  let b:undo_ftplugin .= '|setl fp<'
endif

