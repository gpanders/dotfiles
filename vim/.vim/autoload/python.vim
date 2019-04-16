" Python autoload functions
" Author: Greg Anders

" Generate include path for python, using the terminal's version of Python and
" not Vim's compiled version.  This function is very slow so try to run it
" asynchronously using the jobs API if available; otherwise, just issue a
" system call.
function! python#setup_include_path()
  let pystr = join([
        \ 'import sys',
        \ 'from glob import glob',
        \ 'from os import path',
        \ 'print(",".join(list(filter(lambda d: path.isdir(d) and glob(path.join(d, "*.py")), sys.path))))'],
        \ ';')
  call s:jobstart(['python', '-c', pystr])
endfunction

function! s:callback(...)
  if has('nvim')
    let data = a:2
    let g:python_include_path = data[0]
  elseif has('job')
    let msg = a:2
    let g:python_include_path = msg
  else
    let out = a:1
    let g:python_include_path = out
  endif

  if exists('g:python_include_path')
    let &l:path = &path . ',' . g:python_include_path
  endif
endfunction

function! s:jobstart(cmd, ...)
  " cmd is a list of the form ['python', '-c', '<command string>']
  " for the jobstart/job_start functions, the command string should not be
  " wrapped in quotes but for the system() call, it should
  if has('nvim')
    let opts = {'on_stdout': function('s:callback'), 'stdout_buffered': 1}
    call jobstart(a:cmd, opts)
  elseif has('job')
    let sid = matchstr(expand('<sfile>'), '<snr>\zs\d\+\ze_')
    let opts = {'out_cb': '<snr>' . sid . '_callback', 'in_io': 'null'}
    call job_start(a:cmd, opts)
  else
    " wrap the python command string in quotes and escape quotes in the string
    let cmd = join(a:cmd[0:-2]) . ' "' . escape(a:cmd[-1], '"') . '"'
    let out = systemlist(cmd)
    call s:callback(out[0])
  endif
endfunction
