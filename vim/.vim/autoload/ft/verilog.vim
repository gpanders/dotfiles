let s:debug = '(* MARK_DEBUG="true" *) '

function! ft#verilog#toggle_debug()
  if match(getline('.'), '^\s*' . fnameescape(s:debug)) > -1
    execute 's/' . fnameescape(s:debug) . '/'
  else
    execute 'normal! I' . s:debug . "\<Esc>"
  endif
endfunction
