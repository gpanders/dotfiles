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
  if stridx(&grepprg, '$*') != -1
    let cmd = substitute(&grepprg, '$\*', a:args, 'g')
  else
    let cmd = &grepprg . ' ' . a:args
  endif
  " Run the grep command in a shell to enable shell expansion
  call async#runshell(cmd, {data -> s:callback(a:l, cmd, data)})
endfunction
