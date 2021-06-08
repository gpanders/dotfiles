" This file needs to be in after/plugin/ to ensure that the FileType autocmd
" is defined last and therefore has higher precedence over other FileType
" autocmds
if exists('g:loaded_editorconfig')
    finish
endif
let g:loaded_editorconfig = 1

augroup editorconfig
    autocmd!
    autocmd FileType * call editorconfig#config()
augroup END
