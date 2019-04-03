" xxd filetype
" Author: Greg Anders <greg@gpanders.com>
" Written: 24 Dec 2018

normal! 11|

" Number of bytes per line. xxd default is 16
let b:bytes_per_line = 16

setl cursorline cursorcolumn

" Remap h and l to move byte-wise
noremap <silent> <buffer> h :<C-U>call <SID>ByteLeft()<CR>
noremap <silent> <buffer> l :<C-U>call <SID>ByteRight()<CR>
noremap <silent> <buffer> e :<C-U>call <SID>ByteRight()<CR>l
noremap <silent> <buffer> w :<C-U>call <SID>WordRight()<CR>
noremap <silent> <buffer> W :<C-U>call <SID>WordRight()<CR>
noremap <silent> <buffer> b :<C-U>call <SID>WordLeft()<CR>
noremap <silent> <buffer> B :<C-U>call <SID>WordLeft()<CR>
noremap <silent> <buffer> ge :<C-U>call <SID>ByteLeft()<CR>h
noremap <silent> <buffer> 0 11<Bar>
noremap <silent> <buffer> ^ 11<Bar>

augroup xxd
  autocmd!
  autocmd InsertLeave,TextChanged <buffer> call <SID>Update()
augroup END

let b:undo_ftplugin = 'setl cul< cuc< | au! xxd'

" If &binary is set (with vim -b), use xxd to edit the file like a hex editor
function! s:Update()
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


function! s:ByteLeft()
  let [_, lnum, col, _, _] = getcurpos()
  if col <= 11
    return
  elseif col > 11 && col <= 52
    call search('[A-Za-z0-9]\{2\}', 'b', lnum)
  else
    execute 'normal! h'
  endif
endfunction!

function! s:ByteRight()
  let [_, lnum, col, _, _] = getcurpos()
  if col >= 11 && col < 48
    call search('[A-Za-z0-9]\{2\}', '', lnum)
  elseif col == 48
    execute 'normal! w'
  else
    execute 'normal! l'
  endif
endfunction!

function! s:WordLeft()
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

function! s:WordRight()
  let [_, lnum, col, _, _] = getcurpos()
  if col >= 52
    execute 'normal! 2W'
  else
    execute 'normal! W'
  endif
endfunction
