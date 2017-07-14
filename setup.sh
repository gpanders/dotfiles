#!/bin/bash

shopt -s extglob

mkdir -p $HOME/.vim
cp -vuR $(pwd)/vim/* $HOME/.vim
cp -vu $(pwd)/vimrc $HOME/.vimrc
curl -fLo $HOME/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

cp -vu $(pwd)/tmux.conf $HOME/.tmux.conf

if hash nvim 2>/dev/null; then
    echo "Found neovim installation, adding neovim specific configs"
    mkdir -p $HOME/.config
    cp -vuR $(pwd)/nvim/* $HOME/.config/nvim
    ln -vs $HOME/.vimrc $HOME/.config/nvim/init.vim
    mkdir -p $HOME/.local/share/nvim/site/autoload
    cp $HOME/.vim/autoload/plug.vim $HOME/.local/share/nvim/site/autoload/plug.vim
    ln -vs $HOME/.vim/plugin $HOME/.local/share/nvim/site/plugin
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
        ln -vs "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile##*/}"
    done
    if hash zsh 2>/dev/null; then
        sudo chsh -s $(which zsh) ${SUDO_USER:-$USER}
    else
        echo "zsh not found, not updating default shell"
    fi
else
    echo "Git not found; not cloning prezto"
fi

install_solarized_dircolors = 0
read -p "Install solarized dircolors? [y/N]" ans
case "$ans" in
    [Yy]|[Yy][Ee][Ss] ) install_solarized_dircolors = 1 ;;
    * ) break ;;
esac

if [ install_solarized_dircolors -eq 1 ]; then
    git clone --quiet https://github.com/seebi/dircolors-solarized.git
    cp -uv dircolors-solarized/dircolors.256dark $HOME/.dir_colors
fi

echo " "
echo "Setup complete. You can now delete this directory"
