function! ft#xxd#update()
  " Update the ASCII representation of the data after modifying hex values
  let view = winsaveview()
  let [_, lnum, col, _, _] = getcurpos()
  let line = getline('.')
  let linelen = strlen(line)
  let asciicolstart = linelen - b:bytes_per_line
  if col > asciicolstart
    " ASCII section
    " Place cursor at first non-whitespace character
    call cursor(lnum, asciicolstart)
    let ascii = line[asciicolstart-1:linelen-1]
    for i in range(1, b:bytes_per_line)
      call searchpos('[A-Za-z0-9]\{2\}', 'b', lnum)
      let char = ascii[b:bytes_per_line-i]
      if char !=# '.'
        let nr = printf('%.2x', char2nr(char))
        execute "normal! R" . nr . "\<Esc>h"
      endif
    endfor
  else
    silent %!xxd -r
    silent %!xxd
  endif
  call winrestview(view)
endfunction!


function! ft#xxd#byteleft()
  let [_, lnum, col, _, _] = getcurpos()
  if col <= 11
    return
  elseif col > 11 && col <= 52
    call search('[A-Za-z0-9]\{2\}', 'b', lnum)
  else
    execute 'normal! h'
  endif
endfunction!

function! ft#xxd#byteright()
  let [_, lnum, col, _, _] = getcurpos()
  if col >= 11 && col < 48
    call search('[A-Za-z0-9]\{2\}', '', lnum)
  elseif col == 48
    execute 'normal! w'
  else
    execute 'normal! l'
  endif
endfunction!

function! ft#xxd#wordleft()
  let [_, lnum, col, _, _] = getcurpos()
  if col <= 11
    if lnum > 1
      execute 'normal! 0B'
    else
      return
    endif
  else
    execute 'normal! B'
  endif
endfunction

function! ft#xxd#wordright()
  let [_, lnum, col, _, _] = getcurpos()
  if col >= 52
    execute 'normal! 2W'
  else
    execute 'normal! W'
  endif
endfunction
