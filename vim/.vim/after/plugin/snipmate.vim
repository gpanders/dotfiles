" snipmate
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-10-01

if !get(g:, 'loaded_snips')
    finish
endif

let g:snips_author = 'Greg Anders'

inoremap <silent> <expr> <Tab> pumvisible() ? "\<C-N>" : "\<C-R>=snipmate#snips#TriggerSnippet()\<CR>"
inoremap <silent> <expr> <S-Tab> pumvisible() ? "\<C-P>" : "\<C-R>=snipmate#snips#BackwardsSnippet()\<CR>"
