augroup load_lspconfig
    autocmd!
    autocmd FileType go,c,cpp,rust,python ++once lua require('config.lspconfig')
augroup END
