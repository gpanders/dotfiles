" The argument `str' is the text to replace with. This should be the same as
" whatever the mapping is (is there a way to do this automatically)?
function! jumphome#jump(str)
  let cmdline = getcmdline()
  if match(cmdline[0:1], '\(e[ d]\|sp\|vs\)') == 0
    call feedkeys("\<C-\>e'" . split(cmdline)[0] . " " . a:str . "'\<CR>")
  else
    return a:str
  endif
endfunction
