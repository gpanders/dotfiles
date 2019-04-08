" Create an interactive Python terminal in vim
" Author: Greg Anders <greg@gpanders.com>
" Date: 2018-12-19

if exists('g:loaded_pyterm')
  finish
endif
let g:loaded_pyterm = 1

command -nargs=? Pyterm call pyterm#open(<f-args>)

nnoremap <silent> <Plug>(PytermOpen) :<C-U>Pyterm<CR>
