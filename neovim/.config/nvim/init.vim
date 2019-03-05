set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
let $MYVIMRC = expand('~/.vim/vimrc')
source $MYVIMRC
