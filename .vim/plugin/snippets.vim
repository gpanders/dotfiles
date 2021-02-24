if exists('g:loaded_snippets')
    finish
endif
let g:loaded_snippets = 1

let g:snippets_dir = split(&runtimepath, ',')[0] . '/snippets'

imap <silent> <expr> <Tab> pumvisible() ? "\<C-N>" : "\<C-R>=snippets#expand()\<CR>"
