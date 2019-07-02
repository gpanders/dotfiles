" Create a guard to prevent recursively sourcing this file
if exists('g:did_user_colors')
  finish
endif
let g:did_user_colors = 1

" The :colorscheme command does not work recursively, but the below does
" basically the same thing
runtime! ALL colors/base16-eighties.vim

exe printf('hi User1 term=bold,reverse ctermfg=12 ctermbg=11 guifg=#%s guibg=#%s', g:base16_gui0C, g:base16_gui0B)
exe printf('hi User2 term=bold,reverse ctermfg=8 ctermbg=0 guifg=#%s guibg=#%s', g:base16_gui0C, g:base16_gui0B)
exe printf('hi User3 term=bold,reverse ctermfg=8 ctermbg=0 guifg=#%s guibg=#%s', g:base16_gui0C, g:base16_gui0B)
exe printf('hi User4 term=bold,reverse ctermfg=12 ctermbg=11 guifg=#%s guibg=#%s', g:base16_gui0C, g:base16_gui0B)
exe printf('hi User5 term=bold,reverse ctermfg=12 ctermbg=11 guifg=#%s guibg=#%s', g:base16_gui0C, g:base16_gui0B)

hi link StatuslineModeNormal Normal

exe printf('hi StatuslineModeInsert ctermfg=4 guifg=#%s', g:base16_gui04)
exe printf('hi StatuslineModeReplace ctermfg=1 guifg=#%s', g:base16_gui01)
exe printf('hi StatuslineModeVisual ctermfg=5 guifg=#%s', g:base16_gui05)

exe printf('hi SpellBad ctermbg=NONE ctermfg=1 guifg=#%s', g:base16_gui01)
exe printf('hi SpellCap ctermbg=NONE ctermfg=4 guifg=#%s', g:base16_gui04)
exe printf('hi SpellRare ctermbg=NONE ctermfg=5 guifg=#%s', g:base16_gui05)
exe printf('hi SpellLocal ctermbg=NONE ctermfg=6 guifg=#%s', g:base16_gui06)
exe printf('hi WildMenu ctermfg=15 ctermbg=11 guifg=#%s guibg=#%s', g:base16_gui0F, g:base16_gui0B)

hi Underlined ctermfg=NONE guifg=NONE
