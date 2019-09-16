function! qf#toggle(l)
  if len(filter(getwininfo(), {_, v -> a:l ? v.loclist : v.quickfix}))
    exe a:l ? 'lclose' : 'cclose'
  else
    exe a:l ? 'lopen' : 'copen'
  endif
endfunction
