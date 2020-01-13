" Asynchronous grep

function! s:callback(l, lines) abort
  let F = a:l ? function('setloclist', [0]) : function('setqflist')
  call F([], 'a', {'lines': a:lines})
endfunction

function! s:completed(l) abort
  silent exe 'doautocmd QuickFixCmdPost' a:l ? 'lgrep' : 'grep'
endfunction

" Expand characters from :h cmdline-special
let s:expandable = '\v(^| )(%(\%|#\d*|##|<%(cfile|cword|cWORD|cexpr)>)%(:[p~.htreS])*)%( |$)'

function! grep#grep(l, args) abort
  let args = substitute(a:args, s:expandable, '\=submatch(1) . expand(submatch(2) . '':S'')', '')
  if stridx(&grepprg, '$*') != -1
    let grepcmd = substitute(&grepprg, '$\*', args, 'g')
  else
    let grepcmd = &grepprg . ' ' . args
  endif

  " Run the grep command in a shell to enable shell expansion
  let cmd = split(&shell) + split(&shellcmdflag) + [grepcmd]
  call async#run(cmd, {lines -> s:callback(a:l, lines)}, {'buffered': 0, 'completed': {_ -> s:completed(a:l)}})

  silent exe 'doautocmd QuickFixCmdPre' a:l ? 'lgrep' : 'grep'
  let F = a:l ? function('setloclist', [0]) : function('setqflist')
  call F([], 'r', {'title': grepcmd, 'items': []})
  exe 'botright' a:l ? 'lopen' : 'copen'
endfunction
