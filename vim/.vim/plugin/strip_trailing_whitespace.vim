"
" strip_trailing_whitespace.vim: User command to strip both horizontal and
" vertical whitespace in a buffer, with optional range, reporting both
" accurately and restoring the cursor afterwards.
"
" Author: Tom Ryder <tom@sanctum.geek.nz>
" License: Same as Vim itself
"
if exists('g:loaded_strip_trailing_whitespace') || &compatible
  finish
endif
if !has('user_commands') || v:version < 600
  finish
endif
let g:loaded_strip_trailing_whitespace = 1

" Wrapper function to strip both horizontal and vertical trailing whitespace,
" return the cursor to its previous position, and report changes
function s:Strip(start, end) abort

  " Save cursor position
  let l:line = line('.')
  let l:col = col('.')

  " Whether we made changes
  let l:changed = 0

  " If we're going to the end, strip vertical space; we do this first so we
  " don't end up reporting having trimmed lines that we deleted
  if a:end == line('$')
    let l:vertical = s:StripVertical()
    let l:changed = l:changed || l:vertical > 0
  endif

  " Strip horizontal space
  let l:horizontal = s:StripHorizontal(a:start, a:end)
  let l:changed = l:changed || l:horizontal > 0

  " Return the cursor
  call s:Cursor(l:line, l:col)

  " Report what changed
  let l:msg = l:horizontal.' trimmed'
  if exists('l:vertical')
    let l:msg = l:msg.', '.l:vertical.' deleted'
  endif
  echomsg l:msg

  " Return whether anything changed
  return l:changed

endfunction

" Strip horizontal trailing whitespace, return the number of lines changed
function s:StripHorizontal(start, end) abort

  " Start a count of lines trimmed
  let l:count = 0

  " Iterate through buffer
  let l:num = a:start
  while l:num <= line('$') && l:num <= a:end

    " If the line has trailing whitespace, strip it off and bump the count
    let l:line = getline(l:num)
    if l:line =~# '\s\+$'
      call setline(l:num, substitute(l:line, '\s*$', '', ''))
      let l:count = l:count + 1
    endif

    " Bump for next iteration
    let l:num = l:num + 1

  endwhile

  " Return the number of lines trimmed
  return l:count

endfunction

" Strip trailing vertical whitespace, return the number of lines changed
function s:StripVertical() abort

  " Store the number of the last line we found with non-whitespace characters
  " on it; start at 1 because even if it's empty it's never trailing
  let l:eof = 1

  " Iterate through buffer
  let l:num = 1
  while l:num <= line('$')

    " If the line has any non-whitespace characters in it, update our pointer
    " to the end of the file text
    let l:line = getline(l:num)
    if l:line =~# '\S'
      let l:eof = l:num
    endif

    " Bump for next iteration
    let l:num = l:num + 1

  endwhile

  " Get the number of lines to delete; if there are any, build a range and
  " remove them with :delete, suppressing its normal output (we'll do it)
  let l:count = line('$') - l:eof
  if l:count
    let l:range = (l:eof + 1).',$'
    silent execute l:range.'delete'
  endif

  " Return the number of lines deleted
  return l:count

endfunction

" Position the cursor; use cursor() if we have it, :normal if not (Vim 6.0)
function s:Cursor(line, col) abort
  if exists('*cursor')
    return cursor(a:line, a:col)
  else
    execute 'normal! '.a:line.'G'.a:col.'|'
    return 1
  endif
endfunction

" User command for the above
command! -range=% StripTrailingWhitespace
      \ call <SID>Strip(<line1>, <line2>)
