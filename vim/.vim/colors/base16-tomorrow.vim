" Create a guard to prevent recursively sourcing this file
if exists('g:did_user_colors')
  finish
endif
let g:did_user_colors = 1

" The :colorscheme command does not work recursively, but the below does
" basically the same thing
runtime! ALL colors/base16-tomorrow.vim

highlight User1 term=bold,reverse ctermfg=12 ctermbg=11 guifg=#a09f93 guibg=#515151
highlight User2 term=bold,reverse ctermfg=8 ctermbg=0 guifg=#747369 guibg=#2d2d2d
highlight User3 term=bold,reverse ctermfg=8 ctermbg=0 guifg=#747369 guibg=#2d2d2d
highlight User4 term=bold,reverse ctermfg=12 ctermbg=11 guifg=#a09f93 guibg=#515151
highlight User5 term=bold,reverse ctermfg=12 ctermbg=11 guifg=#a09f93 guibg=#515151

highlight link StatuslineModeNormal Normal
highlight StatuslineModeInsert ctermfg=4 guifg=#6699cc
highlight StatuslineModeReplace ctermfg=1 guifg=#f2777a
highlight StatuslineModeVisual ctermfg=5 guifg=#cc99cc

highlight SpellBad ctermbg=NONE ctermfg=1 guifg=#f2777a
highlight SpellCap ctermbg=NONE ctermfg=4 guifg=#6699cc
highlight SpellRare ctermbg=NONE ctermfg=5 guifg=#cc99cc
highlight SpellLocal ctermbg=NONE ctermfg=6 guifg=#66cccc
highlight WildMenu ctermfg=15 ctermbg=11 guifg=#f2f0ec guibg=#515151
highlight Underlined ctermfg=NONE guifg=NONE

unlet g:did_user_colors
