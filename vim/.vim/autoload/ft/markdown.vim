function! ft#markdown#foldexpr(lnum)
  let head = count(matchstr(getline(a:lnum), '^#\+'), '#')
  return head > 0 ? '>' . head : '='
endfunction
