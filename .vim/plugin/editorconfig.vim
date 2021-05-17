if exists('g:loaded_editorconfig')
    finish
endif
let g:loaded_editorconfig = 1

augroup editorconfig
    autocmd!
    autocmd FileType * call editorconfig#config()
augroup END
