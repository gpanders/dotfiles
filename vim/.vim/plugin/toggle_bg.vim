" Toggle between light and dark background
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-03-27

if exists('g:loaded_toggle_bg') || !has('termguicolors')
  finish
endif
let g:loaded_toggle_bg = 1

function! s:toggle_bg()
  if exists('g:toggle_bg_dark') && exists('g:toggle_bg_light')
    if !&termguicolors
      set termguicolors
    endif

    if g:colors_name ==# g:toggle_bg_dark
      let g:colors_name = g:toggle_bg_light
    else
      let g:colors_name = g:toggle_bg_dark
    endif
    " :syntax enable sources the colorscheme file
    syntax enable
  else
    " If no colorschemes are defined then just toggle the bg setting
    if &bg ==# 'dark'
      set bg=light
    else
      set bg=dark
    endif
  endif
endfunction

command! -nargs=0 ToggleBg call s:toggle_bg()
