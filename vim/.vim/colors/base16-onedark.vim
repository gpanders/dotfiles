" Create a guard to prevent recursively sourcing this file
if exists('g:did_user_colors')
  finish
endif
let g:did_user_colors = 1

" The :colorscheme command does not work recursively, but the below does
" basically the same thing
runtime! ALL colors/base16-onedark.vim

highlight User1 term=bold,reverse ctermfg=12 ctermbg=11 guifg=#565c64 guibg=#3e4451
highlight User2 term=bold,reverse ctermfg=8 ctermbg=0 guifg=#545862 guibg=#282c34
highlight User3 term=bold,reverse ctermfg=8 ctermbg=0 guifg=#545862 guibg=#282c34
highlight User4 term=bold,reverse ctermfg=12 ctermbg=11 guifg=#565c64 guibg=#3e4451
highlight User5 term=bold,reverse ctermfg=12 ctermbg=11 guifg=#565c64 guibg=#3e4451

highlight link StatuslineModeNormal Normal
highlight StatuslineModeInsert ctermfg=4 guifg=#61afef
highlight StatuslineModeReplace ctermfg=1 guifg=#e06c75
highlight StatuslineModeVisual ctermfg=5 guifg=#c678dd

highlight SpellBad ctermbg=NONE ctermfg=1 guifg=#e06c75
highlight SpellCap ctermbg=NONE ctermfg=4 guifg=#61afef
highlight SpellRare ctermbg=NONE ctermfg=5 guifg=#c678dd
highlight SpellLocal ctermbg=NONE ctermfg=6 guifg=#56b6c2
highlight WildMenu ctermfg=15 ctermbg=11 guifg=#c8ccd4 guibg=#3e4451
highlight Underlined ctermfg=NONE guifg=NONE

unlet g:did_user_colors
