" Easily jump to home directory on the command line
" When using any of the edit commands (:edit, :sp, :vs), typing ~/ anywhere in
" the file path will immediately change the path to the user's home directory
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-03-05

if exists('g:loaded_jumphome')
  finish
endif
let g:loaded_jumphome = 1

cnoremap <expr> ~/ jumphome#jump('~/')
