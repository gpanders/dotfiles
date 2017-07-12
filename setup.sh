#!/usr/bin/env bash

cp -r $(pwd)/vim $HOME/.vim
cp $(pwd)/vimrc $HOME/.vimrc
cp $(pwd)/tmux.conf $HOME/.tmux.conf

if hash nvim 2>/dev/null; then
    mkdir -p $HOME/.config
    cp -r $(pwd)/nvim $HOME/.config/nvim
    ln -s $HOME/.vimrc $HOME/.config/nvim/init.vim
fi

if hash git 2>/dev/null; then
    git clone --recursive https://github.com/gpanders/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
    for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/!(README.md); do
        ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile##*/}"
    done
    if hash zsh 2>/dev/null; then
        sudo chsh -s $(which zsh) ${SUDO_USER:-$USER}
    fi
else
    echo "Git not found; not cloning prezto"
fi
