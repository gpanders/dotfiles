" Asynchronous grep
" Author: Greg Anders
" Date: 2019-05-18

function! s:callback(l, lines)
  let F = a:l ? function('setloclist', [0]) : function('setqflist')
  call F([], 'a', {'lines': a:lines})
  exe 'botright' a:l ? 'lwindow' : 'cwindow'
  silent exe 'doautocmd QuickFixCmdPost' a:l ? 'lgrep' : 'grep'
endfunction

function! grep#grep(l, args)
  if stridx(&grepprg, '$*') != -1
    let cmd = substitute(&grepprg, '$\*', a:args, 'g')
  else
    let cmd = &grepprg . ' ' . a:args
  endif

  " Run the grep command in a shell to enable shell expansion
  call async#run(cmd, {lines -> s:callback(a:l, lines)}, {'shell': 1, 'buffered': 0})

  silent exe 'doautocmd QuickFixCmdPre' a:l ? 'lgrep' : 'grep'
  let F = a:l ? function('setloclist', [0]) : function('setqflist')
  call F([], 'r', {'title': cmd, 'items': []})
endfunction
