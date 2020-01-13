" Asynchronous grep

function! s:callback(l, lines) abort
  let F = a:l ? function('setloclist', [0]) : function('setqflist')
  call F([], 'a', {'lines': a:lines})
endfunction

function! s:completed(l) abort
  silent exe 'doautocmd QuickFixCmdPost' a:l ? 'lgrep' : 'grep'
endfunction

function! grep#grep(l, args)
  if stridx(&grepprg, '$*') != -1
    let cmd = substitute(&grepprg, '$\*', a:args, 'g')
  else
    let cmd = &grepprg . ' ' . a:args
  endif

  " Run the grep command in a shell to enable shell expansion
  call async#run(cmd, {lines -> s:callback(a:l, lines)}, {'shell': 1, 'buffered': 0, 'completed': {_ -> s:completed(a:l)}})

  silent exe 'doautocmd QuickFixCmdPre' a:l ? 'lgrep' : 'grep'
  let F = a:l ? function('setloclist', [0]) : function('setqflist')
  call F([], 'r', {'title': cmd, 'items': []})
  exe 'botright' a:l ? 'lopen' : 'copen'
endfunction
