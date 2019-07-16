" Create a guard to prevent recursively sourcing this file
if exists('g:did_user_colors')
  finish
endif
let g:did_user_colors = 1

" The :colorscheme command does not work recursively, but the below does
" basically the same thing
runtime! ALL colors/flattened.vim

if &background ==# 'light'
  hi User1 ctermfg=15 ctermbg=11 guifg=#fdf6e3 guibg=#657b83
  hi User2 ctermfg=15 ctermbg=14 guifg=#fdf6e3 guibg=#93a1a1
  hi User3 ctermfg=11 ctermbg=7 guifg=#657b83 guibg=#eee8d5
  hi User4 ctermfg=15 ctermbg=14 guifg=#fdf6e3 guibg=#93a1a1
  hi StatuslineModeInsert ctermfg=4 guifg=#61afef
  hi StatuslineModeReplace ctermfg=1 guifg=#e06c75
  hi StatuslineModeVisual ctermfg=5 guifg=#c678dd
else
  hi User1 ctermfg=8 ctermbg=12 guifg=#002b36 guibg=#839496
  hi User2 ctermfg=8 ctermbg=10 guifg=#002b36 guibg=#586e75
  hi User3 ctermfg=12 ctermbg=0 guifg=#839496 guibg=#073642
  hi User4 ctermfg=8 ctermbg=10 guifg=#002b36 guibg=#586e75
  hi link StatuslineModeNormal Normal
  hi StatuslineModeInsert ctermfg=4 guifg=#61afef
  hi StatuslineModeReplace ctermfg=1 guifg=#e06c75
  hi StatuslineModeVisual ctermfg=5 guifg=#c678dd
endif

unlet g:did_user_colors
