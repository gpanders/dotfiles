" xxd filetype
" Author: Greg Anders <greg@gpanders.com>
" Written: 24 Dec 2018

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

normal! 11|

" Number of bytes per line. xxd default is 16
let b:bytes_per_line = 16

setl cursorline cursorcolumn
let b:undo_ftplugin .= '|setl cul< cuc<'

" Remap h and l to move byte-wise
noremap <silent> <buffer> h :<C-U>call ft#xxd#byteleft()<CR>
noremap <silent> <buffer> l :<C-U>call ft#xxd#byteright()<CR>
noremap <silent> <buffer> e :<C-U>call ft#xxd#byteright()<CR>l
noremap <silent> <buffer> w :<C-U>call ft#xxd#wordright()<CR>
noremap <silent> <buffer> W :<C-U>call ft#xxd#wordright()<CR>
noremap <silent> <buffer> b :<C-U>call ft#xxd#wordleft()<CR>
noremap <silent> <buffer> B :<C-U>call ft#xxd#wordleft()<CR>
noremap <silent> <buffer> ge :<C-U>call ft#xxd#byteleft()<CR>h
noremap <silent> <buffer> 0 11<Bar>
noremap <silent> <buffer> ^ 11<Bar>

let b:undo_ftplugin .= '|nun <buffer> h'
      \ . '|nun <buffer> l'
      \ . '|nun <buffer> e'
      \ . '|nun <buffer> w'
      \ . '|nun <buffer> W'
      \ . '|nun <buffer> b'
      \ . '|nun <buffer> B'
      \ . '|nun <buffer> ge'
      \ . '|nun <buffer> 0'
      \ . '|nun <buffer> ^'

augroup xxd
  autocmd!
  autocmd InsertLeave,TextChanged <buffer> call ft#xxd#update()
augroup END
let b:undo_ftplugin .= '|au! xxd * <buffer>'
