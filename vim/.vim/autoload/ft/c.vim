let s:compiler = executable('clang') ? 'clang' : 'gcc'

function! s:callback(ft, lines) abort
  let paths = []
  let active = []
  for line in a:lines
    if line =~# '^#include '
      let active = 1
    elseif line =~# '^\S'
      let active = 0
    elseif active
      call add(paths, matchstr(line, '\S\+'))
    endif
  endfor
  let g:{a:ft}_path = join(map(paths, 'simplify(v:val)'), ',')
  call s:path(a:ft)
endfunction

function! s:path(ft)
  let path = g:{a:ft}_path . ',' . &path
  let &l:path = join(uniq(split(path, ',')), ',')

  " If a directory called 'include' exists in the current working directory,
  " add it to the path
  if isdirectory('include')
    setlocal path^=include
  endif

  " Ensure directory of current file is always first on the path
  setlocal path-=.
  setlocal path^=.
endfunction

function! ft#c#set_path(ft)
  let cmd = s:compiler . ' -E -v -x ' . (a:ft ==# 'cpp' ? 'c++' : 'c') . ' /dev/null 2>/dev/null 2>&1'
  if !exists('g:' . a:ft . '_path')
    call async#run(cmd, function('s:callback', [a:ft]), {'shell': 1})
  else
    call s:path(a:ft)
  endif
endfunction
