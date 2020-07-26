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
endfunction

function! ft#c#tags(update) abort
  let compiler = &filetype ==# 'cpp' ? s:cxx : s:cc
  let tagfile = vim#cachedir() . '/tags/' . substitute(expand('%:p'), '/', '%', 'g') . '.tags'
  execute 'setlocal tags+=' . tagfile
  let &l:tags = join(uniq(split(&l:tags, ',')), ',')

  if !filereadable(tagfile) || a:update
    call mkdir(fnamemodify(tagfile, ':h'), 'p')
    let cmd = [compiler, '-M', '-I', 'include', expand('%'),
                \ '|', 'awk', '''gsub(" ", "\n")''',
                \ '|', 'sed', '-e', '''/^\//!d''', '-e', '''/^\\*\s*$/d''',
                \ '|', 'ctags', '-L', '-', '-o', tagfile]
    if &filetype ==# 'c'
      let cmd += ['--c-kinds=+px', '--langmap=c:+.h', '--languages=c']
    else
      let cmd += ['--c++-kinds=+px', '--extras=+q', '--language-force=c++', '--languages=c++']
    endif

    let fullcmd = split(&shell) + split(&shellcmdflag) + [join(cmd)]
    call async#run(fullcmd, '')
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
