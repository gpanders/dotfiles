" Python autoload functions
" Author: Greg Anders

" Generate include path for python, using the terminal's version of Python and
" not Vim's compiled version. Use the asynchronous jobs API if available,
" otherwise make a system call and cache the result.

let s:python_paths = {}

function! python#set_path()
  let cmd = join([
        \ 'import sys',
        \ 'from glob import glob',
        \ 'from os import path',
        \ 'print(",".join(list(filter(lambda d: path.isdir(d) and glob(path.join(d, "*.py")), sys.path))))'],
        \ ';')

  if !has('nvim') && !has('job')
    let cwd = getcwd()
    if !has_key(s:python_paths, cwd)
      let s:python_paths[cwd] = systemlist('python -c "' . escape(cmd, '"') . '"')[0]
    endif
    let &l:path = &path . ',' . s:python_paths[cwd]
  else
    call async#run(['python', '-c', cmd], "let &l:path = &path . ',' . v:val")
  endif
endfunction
