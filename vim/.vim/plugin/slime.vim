" Configuration for vim-slime
" This file is executed BEFORE vim-slime is loaded
" Author: Greg Anders <greg@gpanders.com>
" Date: 2018-12-19

if exists("$TMUX")
  let g:slime_target = "tmux"
  let g:slime_paste_file = tempname()
  let g:slime_default_config = {
        \ "socket_name": split($TMUX, ",")[0],
        \ "target_pane": "{right-of}"
        \ }
elseif has('nvim')
  let g:slime_target = "neovim"
else
  let g:slime_target = "vimterminal"
endif


