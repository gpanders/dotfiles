" Generate include path for python, using the terminal's version of Python and
" not Vim's compiled version. This function is very expensive so the result
" should be cached into a variable and only called if that variable is
" undefined or empty
function! python#include_path()
  let pystr = join([
        \ 'import sys',
        \ 'from glob import glob',
        \ 'from os import path',
        \ 'print(\",\".join(list(filter(lambda d: path.isdir(d) and glob(path.join(d, \"*.py\")), sys.path))))'],
        \ ';')
  let python_include_path = substitute(system('python -c "' . pystr . '"'), '\n', '', '')
  return python_include_path
endfunction
