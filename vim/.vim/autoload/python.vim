" Generate include path for python. This function is very expensive so the
" result should be cached into a variable and only called if that variable is
" undefined or empty
function! python#include_path()
  let pystr = join([
        \ 'import os',
        \ 'import sys',
        \ 'print(\",\".join(list(filter(lambda d: os.path.isdir(d), sys.path))))'],
        \ ';')
  let python_include_path = substitute(system('python -c "' . pystr . '"'), '\n', '', '')
  return python_include_path
endfunction
