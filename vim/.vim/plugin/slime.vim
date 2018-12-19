" Configuration for vim-slime

if exists("$TMUX")
  let g:slime_target = "tmux"
  let g:slime_default_config = {
        \"socket_name": split($TMUX, ",")[0],
        \"target_pane": "{right-of}"
        \}
elseif has('nvim')
  let g:slime_target = "neovim"
else
  let g:slime_target = "vimterminal"
endif


let g:slime_paste_file = tempname()
