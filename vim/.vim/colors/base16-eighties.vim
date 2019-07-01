" Create a guard to prevent recursively sourcing this file
if exists('g:did_user_colors')
  finish
endif
let g:did_user_colors = 1

" The :colorscheme command does not work recursively, but the below does
" basically the same thing
runtime! ALL colors/base16-eighties.vim

highlight User1 term=bold,reverse ctermfg=12 ctermbg=11 guifg=#969896 guibg=#d6d6d6
highlight User2 term=bold,reverse ctermfg=8 ctermbg=0 guifg=#8e908c guibg=#ffffff
highlight User3 term=bold,reverse ctermfg=8 ctermbg=0 guifg=#8e908c guibg=#ffffff
highlight User4 term=bold,reverse ctermfg=12 ctermbg=11 guifg=#969896 guibg=#d6d6d6
highlight User5 term=bold,reverse ctermfg=12 ctermbg=11 guifg=#969896 guibg=#d6d6d6

highlight link StatuslineModeNormal Normal
highlight StatuslineModeInsert ctermfg=4 guifg=#4271ae
highlight StatuslineModeReplace ctermfg=1 guifg=#c82829
highlight StatuslineModeVisual ctermfg=5 guifg=#8959a8

highlight SpellBad ctermbg=NONE ctermfg=1 guifg=#c82829
highlight SpellCap ctermbg=NONE ctermfg=4 guifg=#4271ae
highlight SpellRare ctermbg=NONE ctermfg=5 guifg=#8959a8
highlight SpellLocal ctermbg=NONE ctermfg=6 guifg=#3e999f
highlight WildMenu ctermfg=15 ctermbg=11 guifg=#1d1f21 guibg=#d6d6d6
highlight Underlined ctermfg=NONE guifg=NONE

unlet g:did_user_colors
