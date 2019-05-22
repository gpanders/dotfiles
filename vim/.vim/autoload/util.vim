" Miscellaneous utility functions
" Author: Greg Anders
" Date: 2019-05-22

" Split a string on whitespace but preserve whitespace within quoted strings
" This is useful when sending commands to a shell, e.g. using system() or
" job_start(). Shells typically separate arguments by whitespace but a quoted
" argument is still considered one argument, even with a space
"
" Example:
"   let str = 'this is "a test string"'
"   call util#shellsplit(str)
"     -> ['this', 'is', 'a test string']
"
" Credits: crose on #vim
function! util#shellsplit(str)
  return map(split(a:str, '\%(^\%("[^"]*"\|[^"]\)*\)\@<= '), {_, v -> substitute(v, '^"\|"$', '', 'g')})
endfunction
