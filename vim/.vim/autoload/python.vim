" Generate include path for python. This function is very expensive so the
" result should be cached into a variable and only called if that variable is
" undefined or empty
function! python#include_path()
    let pystr = join([
                \ 'from distutils.sysconfig import get_python_lib',
                \ 'print(get_python_lib())'],
                \ ';')
    let python_include_path = substitute(system('python -c "' . pystr . '"'), '\n', '', '')
    return python_include_path
endfunction
