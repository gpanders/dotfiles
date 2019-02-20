" C
if &filetype !=# 'c'
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
  if g:os ==# 'Darwin'
    setlocal path+=/usr/local/opt/llvm/include/c++/v1
  else
    setlocal path+=/usr/include
  endif
endif

let b:undo_ftplugin .= '|setl cms< cpt< inc< path<'

if executable('clang-format')
  setlocal equalprg=clang-format
  let b:undo_ftplugin .= '|setl ep<'
endif

