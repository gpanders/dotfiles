#!/usr/bin/env bash

mkdir -p $HOME/.config

cp -r $(pwd)/nvim $HOME/.config/nvim
cp -r $(pwd)/vim $HOME/.vim
cp $(pwd)/vimrc $HOME/.vimrc
ln -s $HOME/.vimrc $HOME/.config/nvim/init.vim
cp $(pwd)/tmux.conf $HOME/.tmux.conf

if hash git 2>/dev/null && hash zsh 2>/dev/null; then
    git clone --recursive https://github.com/gpanders/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
    for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md; do
        ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
    done
    chsh -s /bin/zsh
fi
