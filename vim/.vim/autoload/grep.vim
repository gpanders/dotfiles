" Asynchronous grep
" Author: Greg Anders
" Date: 2019-05-18

function! grep#grep(l, args)
  if stridx(&grepprg, '$*') != -1
    let cmd = substitute(&grepprg, '$\*', a:args, 'g')
  else
    let cmd = &grepprg . ' ' . a:args
  endif

  let Callback = a:l
        \ ? {lines -> setloclist(0, [], 'a', {'lines': lines})}
        \ : {lines -> setqflist([], 'a', {'lines': lines})}

  " Run the grep command in a shell to enable shell expansion
  call async#run(cmd, Callback, {'shell': 1, 'buffered': 0})

  if a:l
    lopen
    call setloclist(0, [], 'r', {'title': cmd, 'items': []})
  else
    copen
    call setqflist([], 'r', {'title': cmd, 'items': []})
  endif
endfunction
