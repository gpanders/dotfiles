" Asynchronous grep
" Author: Greg Anders
" Date: 2019-05-18

function! s:callback(l, title, result)
  if a:l
    lgetexpr a:result
    lopen
  else
    cgetexpr a:result
    copen
  endif
  let w:quickfix_title = a:title
endfunction

function! grep#grep(l, args)
  let cmd = split(&grepprg) + util#shellsplit(a:args)
  call async#run(cmd, {data -> s:callback(a:l, join(cmd), data)})
endfunction
