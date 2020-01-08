function! s:bracket(bracket, ch)
  if a:ch ==? 'a'
    let p = ''
  elseif a:ch ==? 'b'
    let p = 'b'
  elseif a:ch ==? 'l'
    let p = 'l'
  elseif a:ch ==? 't'
    let p = 't'
  elseif a:ch ==? 'q'
    let p = 'c'
  else
    return
  endif

  let next = a:bracket ==# '[' ? 0 : 1
  let caps = toupper(a:ch) ==# a:ch
  let cnt = caps || !v:count ? '' : v:count
  let action = (caps ? (next ? 'last' : 'first') : (next ? 'next' : 'prev'))
  return ":\<C-U>" . cnt . p . action . "\<CR>"
endfunction

function! bracket#left(ch)
  return s:bracket('[', a:ch)
endfunction

function! bracket#right(ch)
  return s:bracket(']', a:ch)
endfunction
