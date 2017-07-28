#!/bin/bash

shopt -s extglob

mkdir -p $HOME/.vim
cp -vnR $(pwd)/vim/* $HOME/.vim/
cp -vn $(pwd)/vimrc $HOME/.vimrc
curl -fLo $HOME/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

cp -vn $(pwd)/tmux.conf $HOME/.tmux.conf

if hash nvim 2>/dev/null; then
    echo "Found neovim installation, adding neovim specific configs"
    mkdir -p $HOME/.config/nvim
    cp -vnR $(pwd)/nvim/* $HOME/.config/nvim/
    if [ ! -s $HOME/.config/nvim/init.vim ]; then
        ln -vs $HOME/.vimrc $HOME/.config/nvim/init.vim
    fi

    mkdir -p $HOME/.local/share/nvim/site/autoload
    cp -n $HOME/.vim/autoload/plug.vim $HOME/.local/share/nvim/site/autoload/plug.vim
    if [ ! -s $HOME/.local/share/nvim/site/plugin ]; then
        ln -vs $HOME/.vim/plugin $HOME/.local/share/nvim/site/plugin
    fi
else
    echo "Neovim not found, skipping neovim configs"
fi

if hash git 2>/dev/null; then
    if [ ! -d "${ZDOTDIR:-$HOME}/.zprezto" ]; then
        git clone --quiet --recursive https://github.com/gpanders/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
    else
       echo ".zprezto dir already exists, not cloning git repo"
    fi
    echo "Symlinking prezto runcoms..."
    for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/!(README.md); do
        if [ ! -s "${ZDOTDIR:-$HOME}/.${rcfile##*/}" ]; then
            ln -vs "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile##*/}"
        fi
    done
    if hash zsh 2>/dev/null; then
        sudo chsh -s $(which zsh) ${SUDO_USER:-$USER}
    else
        echo "zsh not found, not updating default shell"
    fi
else
    echo "Git not found; not cloning prezto"
fi

install_solarized_dircolors=0
read -p "Install solarized dircolors? [y/N] " ans
case "$ans" in
    [Yy]|[Yy][Ee][Ss] ) install_solarized_dircolors=1 ;;
    * ) break ;;
esac

if [ $install_solarized_dircolors -eq 1 ]; then
    curl -fLo $HOME/.dir_colors \
        https://raw.githubusercontent.com/seebi/dircolors-solarized/master/dircolors.ansi-universal
fi

echo " "
echo "Setup complete. You can now delete this directory"
