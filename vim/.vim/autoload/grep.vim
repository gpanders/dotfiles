" Asynchronous grep
" Author: Greg Anders
" Date: 2019-05-18

function! s:callback(l, lines)
  if a:l
    call setloclist(0, [], 'a', {'lines': a:lines})
  else
    call setqflist([], 'a', {'lines': a:lines})
  endif
  exe a:l ? 'lwindow' : 'cwindow'
  exe 'silent doautocmd QuickFixCmdPost' a:l ? 'lgrep' : 'grep'
endfunction

function! grep#grep(l, args)
  if stridx(&grepprg, '$*') != -1
    let cmd = substitute(&grepprg, '$\*', a:args, 'g')
  else
    let cmd = &grepprg . ' ' . a:args
  endif

  " Run the grep command in a shell to enable shell expansion
  call async#run(cmd, {lines -> s:callback(a:l, lines)}, {'shell': 1, 'buffered': 0})

  exe 'silent doautocmd QuickFixCmdPre' a:l ? 'lgrep' : 'grep'
  if a:l
    call setloclist(0, [], 'r', {'title': cmd, 'items': []})
  else
    call setqflist([], 'r', {'title': cmd, 'items': []})
  endif
endfunction
