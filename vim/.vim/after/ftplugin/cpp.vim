" C++ filetype plugin
" Author: Greg Anders <greg@gpanders.com>

if &filetype !=# 'cpp'
  finish
endif

" Set comment string
setlocal commentstring=//%s

" Include macros in completion
setlocal complete+=i,d

" Set include pattern
setlocal include=^\\s*#\\s*include

" Use improved :Man command as keywordprg
setlocal keywordprg=:Man

" Include headers on Unix
if has('unix')
  if g:os ==# 'Darwin'
    setlocal path+=/usr/local/opt/llvm/include/c++/v1
  else
    setlocal path+=/usr/include
  endif
endif

let b:undo_ftplugin .= '|setl cms< cpt< inc< path< kp<'

if executable('clang-format')
  setlocal formatprg=clang-format
  let b:undo_ftplugin .= ' fp<'
endif

