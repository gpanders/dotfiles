function! s:load()
    packadd nvim-compe
    if !exists('g:loaded_compe')
        finish
    endif

    runtime! after/plugin/compe.vim

    call compe#setup({'source': {'path': 1, 'nvim_lsp': 1, 'nvim_lua': 1}})

    set completeopt=menuone,noselect
    set shortmess+=c

    inoremap <silent> <expr> <CR> compe#confirm('<CR>')
    inoremap <silent> <expr> <C-E> compe#close('<C-E>')

    doautocmd <nomodeline> InsertEnter
endfunction

augroup my_compe
    autocmd! InsertEnter * ++once call s:load()
augroup END
