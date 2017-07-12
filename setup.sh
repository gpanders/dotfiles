#!/bin/bash

shopt -s extglob

echo "Copying vim config to $HOME/.vim..."
cp -v -r $(pwd)/vim $HOME/.vim

echo "Copying .vimrc to $HOME/.vimrc..."
cp -v $(pwd)/vimrc $HOME/.vimrc

echo "Copying tmux.conf to $HOME/.tmux.conf..."
cp -v $(pwd)/tmux.conf $HOME/.tmux.conf

if hash nvim 2>/dev/null; then
    echo "Found neovim installation, adding neovim specific configs..."
    mkdir -v -p $HOME/.config
    cp -v -r $(pwd)/nvim $HOME/.config/nvim
    ln -v -s $HOME/.vimrc $HOME/.config/nvim/init.vim
else
    echo "Neovim not found, skipping neovim configs"
fi

if hash git 2>/dev/null; then
    if [ ! -d "${ZDOTDIR:-$HOME}/.zprezto" ]; then
        git clone --recursive https://github.com/gpanders/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
    else
       echo ".zprezto dir already exists, not cloning git repo"
    fi
    echo "Symlinking prezto runcoms..."
    for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/!(README.md); do
        ln -v -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile##*/}"
    done
    if hash zsh 2>/dev/null; then
        sudo chsh -s $(which zsh) ${SUDO_USER:-$USER}
    else
        echo "zsh not found, not updating default shell"
    fi
else
    echo "Git not found; not cloning prezto"
fi

echo " "
echo "Setup complete. You can now delete this directory"
