#!/bin/bash

shopt -s extglob

curr_dir=$(pwd)

if [ -d $HOME/.vim ]; then
  mv $HOME/.vim $HOME/.vim.bak
fi
ln -s $curr_dir/vim $HOME/.vim

if [ -d $HOME/.tmux ]; then
  mv $HOME/.tmux $HOME/.tmux.bak
fi
ln -s $curr_dir/tmux $HOME/.tmux

if [ -f $HOME/.vimrc ]; then
  mv $HOME/.vimrc $HOME/.vimrc.bak
fi
ln -s $curr_dir/vimrc $HOME/.vimrc

if [ -f $HOME/.tmux.conf ]; then
  mv $HOME/.tmux.conf $HOME/.tmux.conf.bak
fi
ln -s $curr_dir/tmux.conf $HOME/.tmux.conf

mkdir -p $HOME/.config/nvim
if [ -f $HOME/.config/nvim/init.vim ]; then
  mv $HOME/.config/nvim/init.vim $HOME/.config/nvim/init.vim.bak
fi
ln -s $curr_dir/nvim/init.vim $HOME/.config/nvim/init.vim

# Upgrade vim-plug and install vim plugins
vim -c PlugUpgrade -c PlugInstall -c q

if hash zsh 2>/dev/null; then
  install_prezto=0
  read -r -p "Install prezto? [y/N] " ans
  if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
    install_prezto=1
  fi

  if [ $install_prezto -eq 1 ]; then
    if hash git 2>/dev/null; then
      if [ ! -d "${ZDOTDIR:-$HOME}/.zprezto" ]; then
        git clone --quiet --recursive https://github.com/gpanders/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
        cd "${ZDOTDIR:-$HOME}"/.zprezto && git submodule update --init --recursive && cd -
      else
        echo ".zprezto dir already exists, not cloning git repo"
      fi
      for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/!(README.md); do
        if [ ! -s "${ZDOTDIR:-$HOME}/.${rcfile##*/}" ]; then
            ln -vs "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile##*/}"
        fi
      done
    else
      echo "Git not found; not cloning prezto"
    fi
  fi

  read -r -p "Change default shell to zsh? [y/N] " ans
  if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
    if ! [[ "$SHELL" =~ "zsh" ]]; then
      sudo chsh -s $(which zsh) ${SUDO_USER:-$USER}
    fi
  fi
fi

read -r -p "Use .bash_aliases file? [y/N] " ans
if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
  cp $curr_dir/bash_aliases $HOME/.bash_aliases
fi

if [ ! -d "$HOME/.pyenv" ]; then
  install_pyenv=0
  read -r -p "Install pyenv? [y/N] " ans
  if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
    install_pyenv=1
  fi

  if [ $install_pyenv -eq 1 ]; then
    if [[ "$OSTYPE" == darwin* ]]; then
      brew install pyenv pyenv-virtualenv
    elif [[ "$OSTYPE" == linux-gnu ]]; then
      git clone https://github.com/pyenv/pyenv.git $HOME/.pyenv
      git clone https://github.com/pyenv/pyenv-virtualenv.git $HOME/.pyenv/plugins/pyenv-virtualenv
    fi
  fi
fi

read -r -p "Install custom fzf config? [y/N] " ans
if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
  cp $curr_dir/fzf.bash $HOME/.fzf.bash
fi

read -r -p "Install solarized dircolors? [y/N] " ans
if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
  curl -fsLo $HOME/.dir_colors \
    https://raw.githubusercontent.com/seebi/dircolors-solarized/master/dircolors.ansi-universal
fi

if hash xmodmap 2>/dev/null; then
  read -r -p "Use .xmodmap file to set Caps Lock to Control? [y/N] " ans
  if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
    if [ -f "$HOME/.xmodmap" ]; then
      cat $curr_dir/xmodmap >> $HOME/.xmodmap
    else
      cp $curr_dir/xmodmap $HOME/.xmodmap
    fi
  fi
fi

echo " "
echo "Setup complete."
