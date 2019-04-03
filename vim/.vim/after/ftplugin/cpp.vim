" cpp filetype plugin
" Author: Greg Anders <greg@gpanders.com>

if isdirectory('/Library/Developer/CommandLineTools')
  let &l:path = glob('/Library/Developer/CommandLineTools/usr/include/c++/*') . ',' . &path
endif
