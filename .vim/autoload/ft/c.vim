let s:cc = exists('$CC') ? expand('$CC') : 'cc'
let s:cxx = exists('$CXX') ? expand('$CXX') : 'c++'

function! s:callback(ft, lines) abort
  let paths = []
  for line in a:lines
    if line =~# '^ '
      call add(paths, matchstr(line, '\S\+'))
    endif
  endfor
  let g:{a:ft}_path = join(map(paths, 'simplify(v:val)'), ',')
  call s:path(a:ft)
endfunction

function! s:path(ft) abort
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

  " Add tags file for compiler
  call s:tags(a:ft)
endfunction

function! s:tags(ft) abort
  let compiler = a:ft ==# 'cpp' ? s:cxx : s:cc
  let machine = systemlist(compiler . ' -dumpmachine')[0]
  let tagfile = vim#cachedir() . '/tags.' . machine
  let &l:tags .= ',' . tagfile

  if !filereadable(tagfile)
    let cmd = ['ctags', '-R', '-o', tagfile] + split(g:{a:ft}_path, ',')
    if a:ft ==# 'c'
        let cmd += ['--c-kinds=+p', '--langmap=c:.h', '--languages=c']
    else
        let cmd += ['--c++-kinds=+p', '--languages=c++']
    endif
    let cmd += split(g:{a:ft}_path, ',')
    call async#run(cmd, '')
  endif
endfunction

function! ft#c#set_path() abort
  let compiler = &filetype ==# 'cpp' ? s:cxx : s:cc
  let cmd = compiler . ' -E -Wp,-v -x' . (&filetype ==# 'cpp' ? 'c++' : 'c') . ' /dev/null 2>&1'
  if !exists('g:' . &filetype . '_path')
    let cmd = split(&shell) + split(&shellcmdflag) + [cmd]
    call async#run(cmd, function('s:callback', [&filetype]))
  else
    call s:path(&filetype)
  endif
endfunction
