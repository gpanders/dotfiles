" easyterm
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-11-13

if exists('g:loaded_easyterm')
    finish
endif
let g:loaded_easyterm = 1

command! -nargs=? Term call easyterm#open(<q-mods>, <q-args>)
