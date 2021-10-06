if exists('did_load_filetypes')
  finish
endif

function! s:checkheader()
  " Use a heuristic that an include statement without a .h extension is a C++
  " header rather than a C header
  if search('\C^#include <[^>.]\+>$', 'nw')
    setfiletype cpp
  else
    setfiletype c
  endif
endfunction

augroup filetypedetect
  autocmd! BufRead,BufNewFile *.bd                            setf json
  autocmd! BufRead,BufNewFile *.vho                           setf vhdl
  autocmd! BufRead,BufNewFile ~/.local/share/zsh/functions/*  setf zsh
  autocmd! BufRead,BufNewFile *.env                           setf env
  autocmd! BufRead,BufNewFile *.cl                            setf opencl
  autocmd! BufRead,BufNewFile *.csv                           setf csv
  autocmd! BufRead,BufNewFile *.h                             call s:checkheader()
augroup END
