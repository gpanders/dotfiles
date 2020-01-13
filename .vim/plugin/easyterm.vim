if exists('g:loaded_easyterm') || !(has('terminal') || has('nvim'))
    finish
endif
let g:loaded_easyterm = 1

command! -nargs=? Term call easyterm#open(<q-mods>, <q-args>)
