" Python autoload functions
" Author: Greg Anders

" Generate include path for python, using the terminal's version of Python and
" not Vim's compiled version. Use the asynchronous jobs API if available,
" otherwise make a system call and cache the result.

let s:python_paths = {}

let s:cmd = join([
      \ 'import sys',
      \ 'from glob import glob',
      \ 'from os import path',
      \ 'print(",".join([d for d in sys.path if path.isdir(d) and glob(path.join(d, "*.py"))]))'],
      \ ';')

function! ft#python#set_path(...)
  let python = 'python'
  if a:0 && !empty(a:1)
    let python = a:1
  endif

  if has('nvim') || has('job')
    call async#run([python, '-c', s:cmd], 'let &l:path = &path . '','' . v:val')
  else
    let cwd = getcwd()
    if !has_key(s:python_paths, cwd)
      let s:python_paths[cwd] = systemlist(python . ' -c "' . escape(s:cmd, '"') . '"')[0]
    endif
    let &l:path = &path . ',' . s:python_paths[cwd]
  endif
endfunction

function! ft#python#pydoc(...)
  let keyword = a:0 ? a:1 : expand('<cword>')
  let tmpfile = tempname()
  call writefile(systemlist('pydoc ' . keyword), tmpfile, '')
  pedit +set\ ro\ nobl\ bt=nofile `=tmpfile`
endfunction
