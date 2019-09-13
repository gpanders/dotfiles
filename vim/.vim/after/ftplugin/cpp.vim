" cpp filetype plugin
" Author: Greg Anders <greg@gpanders.com>

if isdirectory('/usr/include/c++')
  let &l:path = glob('/usr/include/c++/*', 0, 1)[-1] . ',' . &path
elseif isdirectory('/Library/Developer/CommandLineTools/usr/include/c++')
  let &l:path = glob('/Library/Developer/CommandLineTools/usr/include/c++/*', 0, 1)[-1] . ',' . &path
endif

" Ensure directory of current file is always first on the path
setlocal path-=.
setlocal path^=.
