if !get(g:, 'loaded_compe')
    finish
endif

set completeopt=menuone,noselect
set shortmess+=c

inoremap <silent> <expr> <CR> compe#confirm('<CR>')
inoremap <silent> <expr> <C-E> compe#close('<C-E>')

autocmd InsertEnter * ++once
            \ call compe#setup({'source': {'path': 1, 'nvim_lsp': 1, 'nvim_lua': 1}}) |
            \ doautocmd <nomodeline> compe InsertEnter
