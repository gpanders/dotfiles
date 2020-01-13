" strip_trailing_whitespace.vim: User command to strip both horizontal and
" vertical whitespace in a buffer, with optional range, reporting both
" accurately and restoring the cursor afterwards.
"
" Author: Tom Ryder <tom@sanctum.geek.nz>
" License: Same as Vim itself

if exists('g:loaded_strip_trailing_whitespace') || &compatible
  finish
endif
if !has('user_commands') || v:version < 600
  finish
endif
let g:loaded_strip_trailing_whitespace = 1

" User command for the above
command! -range=% StripTrailingWhitespace
      \ call strip_trailing_whitespace#strip(<line1>, <line2>)
