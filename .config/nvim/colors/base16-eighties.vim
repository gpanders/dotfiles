" Create a guard to prevent recursively sourcing this file
if exists('g:did_user_colors')
    finish
endif
let g:did_user_colors = 1

" The :colorscheme command does not work recursively, but the below does
" basically the same thing
runtime! ALL colors/base16-eighties.vim

hi link StatuslineModeNormal Normal
hi Underlined ctermfg=NONE guifg=NONE

" Make all control flow the same color
hi! link Repeat Conditional

call g:Base16hi('User1', g:base16_gui04, g:base16_gui02, g:base16_cterm04, g:base16_cterm02)
call g:Base16hi('User2', g:base16_gui03, g:base16_gui00, g:base16_cterm03, g:base16_cterm00)
call g:Base16hi('User3', g:base16_gui03, g:base16_gui00, g:base16_cterm03, g:base16_cterm00)
call g:Base16hi('User4', g:base16_gui04, g:base16_gui02, g:base16_cterm04, g:base16_cterm02)

call g:Base16hi('StatuslineModeInsert', g:base16_gui0D, '', g:base16_cterm0D, '')
call g:Base16hi('StatuslineModeReplace', g:base16_gui08, '', g:base16_cterm08, '')
call g:Base16hi('StatuslineModeVisual', g:base16_gui0E, '', g:base16_cterm0E, '')

call g:Base16hi('TabLine', g:base16_gui08, '', g:base16_cterm04, g:base16_cterm01)
call g:Base16hi('TabLineSel', g:base16_gui08, '', g:base16_cterm05, g:base16_cterm02)
call g:Base16hi('TabLineIndex', g:base16_gui08, '', g:base16_cterm08, g:base16_cterm01)
call g:Base16hi('TabLineIndexSel', g:base16_gui08, '', g:base16_cterm08, g:base16_cterm02)

unlet g:did_user_colors
