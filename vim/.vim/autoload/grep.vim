function! s:grep(l, args)
  let l:cmd = &grepprg . ' ' . a:args
  if a:l
    lgetexpr system(l:cmd)
    lopen
  else
    cgetexpr system(l:cmd)
    copen
  endif
  let w:quickfix_title = l:cmd
endfunction

function! grep#grep(args)
  call s:grep(0, a:args)
endfunction

function! grep#lgrep(args)
  call s:grep(1, a:args)
endfunction
