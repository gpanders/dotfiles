function! cursorline#toggle(enable)
  if a:enable
    if index(g:cursorline_blacklist, &ft) < 0 && exists('b:cul')
      let &l:cursorline = b:cul
    endif
  else
    if index(g:cursorline_blacklist, &ft) < 0
      let b:cul = &l:cursorline
      let &l:cursorline = 0
    endif
  endif
endfunction
