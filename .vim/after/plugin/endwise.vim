if !get(g:, 'loaded_endwise')
    finish
endif

" Use <CR> to make a selection in popup menu
inoremap <silent> <expr> <CR> pumvisible() ? "\<C-Y>" : "\<C-]><C-G>u\<CR>\<C-R>=EndwiseDiscretionary()\<CR>"
